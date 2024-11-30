#before: nMinInOptModsGen <- rep(1, nGens);nAddedBestModsGen = rep(5, nGens), nModsGen <- rep(10, nGens)
#test1: nMinInOptModsGen <- c(1, 1, 1, rep(3, nGens - 3));nAddedBestModsGen = rep(2, nGens) #making sure that the algo does not converge too soon, nModsGen <- rep(10, nGens)
#test2: nMinInOptModsGen <- c(1, 1, 1, rep(3, nGens - 3));nAddedBestModsGen = rep(3, nGens) #making sure that the algo does not converge too soon, nModsGen <- rep(10, nGens)
#test3: nMinInOptModsGen <- c(1, 1, 1, rep(3, nGens - 3));nAddedBestModsGen = c(rep(3, 5), rep(5, nGens - 5)),  #making sure that the algo does not converge too soon, nModsGen <- rep(15, nGens)
#test4: nMinInOptModsGen <- c(1, 1, 1, rep(3, nGens - 3));nAddedBestModsGen = c(rep(3, 5), rep(5, nGens - 5)),  #making sure that the algo does not converge too soon, nModsGen <- rep(30, nGens)
#test5: nMinInOptModsGen <- rep(1, nGens);nAddedBestModsGen = c(rep(3, 5), rep(5, nGens - 5)),  #making sure that the algo does not converge too soon, nModsGen <- rep(30, nGens)

#adapting nMinInOptModsGen made the algo fit much faster since we reduce the model complexity over the generations (some covars are noise only, and they are just thrown out if not useful)
#nAddedBestModsGen needs to be high enoug (was not the case for test1) to make sure that the algo converges
#nModsGen needs to be high enough to make sure that there is enough randomness in the population (but not too big -> elsewise it will take too long to fit)

rm(list = ls())


# Loading packages and functions ------------------------------------------

source(file.path("R","Packages.R"))
source(file.path("R", "intGAFuncs.R"))

# functions for the interaction terms
source(file.path("R", "allOrderIntTerms.R"))
#TODO: replace the function "allFirstOrderIntTerms" with "allOrderIntTerms"


# Load Data ---------------------------------------------------------------

inputDT <- readRDS(file.path("Data", "inputDT.rds"))

inputDT[, c('polNumb', 'claimNumbMD', 'claimSizeMD', 'claimSizeBI', 'age', 'density', 'carVal') := NULL]
setnames(inputDT, 'claimNumbBI', 'claimNumber')
setnames(inputDT, 'ageGrouped', 'age')
setnames(inputDT, 'carValGrouped', 'carVal')
setnames(inputDT, 'densityGrouped', 'density')

# claimNumbBI ~ s(age) + s(density) + s(carVal) + uwYear + gender + carType + carCat + job + group1 + bm + nYears + cover + offset(exposure)

isFactorDT(inputDT, ,T)
isNumericDT(inputDT, ,T)

vars <- c('age', 'density', 'carVal', 'uwYear', 'gender', 'carType', 'carCat', 'job', 'group1', 'bm', 'nYears', 'cover')
varsGA <- c(vars, allFirstOrderIntTerms(vars))

trainSamp <- sample(c(1:nrow(inputDT)), round(0.8*nrow(inputDT)), replace = FALSE)
testSamp <- createTestSet(inputDT[,.SD,.SDcol = unique(vars)], trainSamp, (nrow(inputDT) - length(trainSamp)))

trainDT <- inputDT[trainSamp, ]
testDT <- inputDT[testSamp, ]

inputDTAll <- copy(inputDT)
inputDT <- copy(trainDT)


# Initialization GA -------------------------------------------------------

seedNumb <- 713
set.seed(seedNumb)

nGens <- 20
nModsGen <- rep(30, nGens)
nCrossOverGen <- rep(1, nGens)
nMutsGen <- rep(1, nGens)
nVarInit <- 10
nVarMax <- 21
nRedMods <- 5 
trainPerc <- 0.5
testPerc <- NULL
valOffSet <- 'exposure'
#valOffSet <- NULL
# nMinInOptModsGen <- c(0,0,0,0,0,0,0,1,0,0)
# nMinInOptModsGen <- NULL
nMinInOptModsGen <- rep(1, nGens)
includeIntsGen <- rep(FALSE, nGens)
# nModsIntGen <- rep(c(0,2), nGens/2)
nModsIntGen <- rep(0, nGens)
# nMaxIntGen <- rep(c(0,5), nGens/2)
nMaxIntGen <- rep(0, nGens)

nAddedBestModsGen <- c(rep(3, 5), rep(5, nGens - 5))

#added 
ratioConcProbMSE <- 0 #1 = MSE, 0 = concProb
ilst2Save <- NULL
distMod <- 'poisson'

statusList <- NULL
upperBoundPred <- NULL

# badOnes <- c('privateUse*coverType', 'coverType*privateUse', 'age*experience', 'experience*age')
badOnes <- c()
saveStatus <- TRUE
partialSaveName <- NULL

#
# Genetic algorithm function
#

# inputDT = data.table object : Our database
#     At this point, only categorical vars are accepted.

# varsGA = the vars that will be selected or not in the genetic algo (exposure, claimSize et claimNumber not included).

# nVarInit = Number of vars used during the initialization phase

# nVarMax = Maximum number of vars during each generation

# nModsGen = Number of models for each generation (needs to be pair number, length = nGens and the numbers can only be monotonuously decreasing)

# nGens = Number of generations

# nCrossOverGen = Number of cross-overs (length = nGens)

# nMutsGen = Number of mutations (length = nGens)

# distMod : selected distribution model. Possibilities are 'poisson', gamma' and 'gaussian'. 

# trainPerc : percentage of inputDT that is included in the training data set (at each generation, another split is made)

# testPerc : percentage of inputDT that is included in the training data set. If equal to NULL, then the complement of the training set is used. 
# If trainPerc = 1, then testPerc = 1 as well.

# nRedMods : For models with more than nVarMax variables, random sample of nVarMax vars are taken from the selected set, 
# and this is repeated nRedMods times. The model (and their corresponding selection of vars) with the highest C-index is retained.

# nAddedBestModsGen : Of all the past models, that have a C-index higher than the best model of the current generation, nAddedBestModsGen are sampled
# and this number of models is in the running to be transmitted to the next generation (to improve the convergence of the GA). 
# A random sample is taken from the best models and the ones of the current generation. (length = nGens)

# valOffSet : This variable allows one to set the offset (set it equal to 'exposure'), being exposure in most cases (the right link function is automatically added to it). 
# Note that if disModel is gamma or gaussian, this argument should be set to NULL. 

# nMinInOptModsGen : The minimal number of times a variable should appear in the set of final models of the generation and in all the model of the previous generations 
# that had a higher value in concProb than the concProb of the best model of the current generation (length = nGens). The main idea is that this number should increase  
# for later generations, as this will force the GA to converge more quickly.

# includeIntsGen : Boolean vector that indicates whether or not interactions are taken into account for the given generation. When equal to NULL, no interactions are 
# taken into account. Note that interactions are never considered for the init phase (length = nGens)

# nModsIntGen: Number of models with randomly sampled interactions terms that are considered by the GA (we evaluate the effect of interaction effects by randomly 
# adding interaction terms to the main effects and to just consider the model with the highest concProb next). (length = nGens)

# nMaxIntGen: Maximum number of interaction terms that are added to the main effects, when testing nModsIntGen. When it equals NULL, 
# no interaction terms are considered. (length = nGens)

# saveStatus Boolean indicating whether or not the output of each generation should be saved or not (statusList). 

# statusList Character vector that indicates which of the next outputs would be interesting to save:
#             list(matBinInit = matBinInit, concProbInit = concProbInit,
#             matMutFin = matMutFin, matCrossOverFin = matCrossOverFin, matNewFitFin = matNewFitFin, modsIterFin = modsIterFin,
#             newModsIterFin = newModsIterFin, concProbIterFin = concProbIterFin,
#             concProbWgtsIterFin = concProbWgtsIterFin, varsGAFin = varsGAFin, minimalSet = minimalSet,
#             intModsFin = intModsFin, newvalOffSet = inputDT$newvalOffSet, iGen = iGen, MSEFull, MSENull, MSEFullNew, CPFull, CPNull, CPFullNew)
# If it is equal to NULL, then the above list is constructed. 

# ratioConcProbMSE: Ratio between the concProb and MSE for the fitting criterion. When the value is equal to 0, only the concProb is taken into account. 
# When its value is different from 0 or 1, the normalized value for the concProb and the MSE is used. 

# upperBoundPred : when dealing with (strictly positive) continuous data, it is better to sometimes just truncate the predictions, to avoid values that do not 
# make economical sense. The default value is NULL, meaning that there is no truncation applied to the predictions.

functionGA <- function(inputDT, varsGA, nModsGen, nGens, nCrossOverGen, nMutsGen,
                       nVarInit, nVarMax, nRedMods = 5, trainPerc = 0.8, testPerc = NULL,
                       distMod = 'poisson', 
                       nAddedBestModsGen = rep(5, nGens), valOffSet = 'exposure',
                       nMinInOptModsGen = NULL, 
                       includeIntsGen = NULL, nModsIntGen = NULL, nMaxIntGen = NULL, 
                       saveStatus = TRUE, partialSaveName = NULL, 
                       statusList = NULL,
                       typeThreshold = NULL,
                       ratioConcProbMSE = NULL,
                       upperBoundPred = NULL, badOnes = NULL){
  
  nModelsInit <- nModsGen[1]
  
  #TODO: make it that it is given to the function what distribution needs to be given, so that we can chose it, like a tweedie if necessary
  if(distMod == 'poisson'){
    distMod <- poisson(link='log')
  } else if (distMod== 'gamma'){
    distMod <- Gamma(link = 'log')
  } else if (distMod == 'gaussian'){
    distMod <- gaussian(link = 'identity')
  } else if (distMod == 'binomial'){
    distMod <- binomial(link = "logit")
  } else {
    stop('distMod should be poisson, gamma, gaussian or binomial.')
  }
  
  #TODO: the person needs to give the proper column to look at the "observed"
  if(distMod$family[1] == 'poisson'){
    checkDT(inputDT, 'claimNumber')
    inputDT[, observed := claimNumber]
  } else if (distMod$family[1] != 'binomial'){
    checkDT(inputDT, 'claimSize')
    inputDT[, observed := claimSize]
  } else { # distMod$family[1] = binomial -> Proba.
    checkDT(inputDT, 'claimSizeProb')
    inputDT[, observed := claimSizeProb]
    inputDT[, exposure := 1] 
  }
  
  ###  Checks input params
  
  checkCharVec(list(varsGA)); checkDT(inputDT, unique(unlist(strsplit(varsGA, '[*]')))) 
  checkNumOrIntVec(list(nModelsInit)); checkLength(list(nModelsInit), 1); checkRanges(list(nModelsInit), list(c('>',0)))
  checkNumOrIntVec(list(nModsGen)); checkLength(list(nModsGen), nGens); for(iList in 1:length(nModsGen)) checkRanges(as.list(nModsGen)[iList], list(c('>',0)))
  checkNumOrIntVec(list(nCrossOverGen)); checkLength(list(nCrossOverGen), nGens); for(iList in 1:length(nCrossOverGen)) checkRanges(as.list(nCrossOverGen)[iList], list(c('>',0)))
  checkNumOrIntVec(list(nMutsGen)); checkLength(list(nMutsGen), nGens); for(iList in 1:length(nMutsGen)) checkRanges(as.list(nMutsGen)[iList], list(c('>',0)))
  checkNumOrIntVec(list(nVarInit)); checkLength(list(nVarInit), 1); checkRanges(list(nVarInit), list(c('>', 0,'<=',length(varsGA))))
  checkNumOrIntVec(list(nVarMax)); checkLength(list(nVarMax), 1); checkRanges(list(nVarMax), list(c('>', 0,'<=',length(varsGA))))
  
  if(!is.null(nMinInOptModsGen)){
    checkNumOrIntVec(list(nMinInOptModsGen))
    checkLength(list(nMinInOptModsGen), nGens)
    for(iList in 1:length(nMinInOptModsGen)) checkRanges(as.list(nMinInOptModsGen)[iList], list(c('>=',0)))
  }
  
  if((sum(nModsGen%%2)>0) || (nModelsInit%%2 == 1)){ 
    stop('All element of nModsGen/nModelsInit should be even.')
  } else if (all(isFactorDT(inputDT, unique(unlist(strsplit(varsGA, '[*]'))))) == FALSE){
    stop('All covariate in varsGA should be categorical (except exposure, claimSize and claimNumber)')
  }
  
  if(!all(diff(c(nModelsInit,nModsGen)) > 0)){
    stop('Number of models should be decreasing : nModsGen[i] <= nModsGen[i-1]')
  }
  
  # for(iLengMod in 1:length(nModsGen)){
  #   if(iLengMod == 1){
  #     if(nModsGen[iLengMod] > nModelsInit){
  #       stop('Number of model should be decreasing : nModsGen[1] <= nModelsInit')
  #     }
  #   }
  #   else{
  #     if(nModsGen[iLengMod] > nModsGen[iLengMod-1]){
  #       stop('Number of model should be decreasing : nModsGen[counter] <= nModsGen[counter-1]')
  #     }
  #   }
  # }
  
  checkLength(list(nAddedBestModsGen), nGens)
    
  #if not enough obs: test and training sample are the same
  varLevs <- extractLevelDT(inputDT[,.SD,.SDcol = c(unique(c(unlist(strsplit(unlist(varsGA), '[*]')))))])
  nMaxVarLevs <- max(unlist(llply(1:100000, function(xx) sum(length(unlist(varLevs[sample(1:length(c(varsGA)), nVarMax)]))) - 1))*5)
  if(nrow(inputDT)*trainPerc*1.05 < (nMaxVarLevs)){ # * 1.05 to create a small margin
    trainPerc <- 1
    cat('Not enough observations were observed for the number of levels that are considered, such that the test and training set are the same (trainPerc = testPerc = 1). \n') 
  }
    
  ###  Init: populating statusList, defining relevant vars (concProb/MSE null and full model, defining training/test set)
   
  if(is.null(statusList)){
    
    modsIterFin <- list() # list of all models of every iteration
    newModsIterFin <- list() # list of the optimal models
    concProbIterFin <- list() # list of the concProb of all models of every iteration
    concProbWgtsIterFin <- list() # list of the weights generated from the concProb of all models of every iteration
    matCrossOverFin <- list() # list of crossover matrices 
    matMutFin <- list() # list of mutation matrices
    matNewFitFin <- list() # list of the matrices after having computed the concProb
    varsGAFin <- list() # list of vars of every iteration: note that the order of the vars changes for the var string for every iteration
    intModsFin <- list() # list of interactions of every iteration
    
  	#computing MSE/concProb the null and full model
    
    #defining training et test set to compute MSE/concProb the null and full model
    if(trainPerc == 1){
      trainSamp <- 1:nrow(inputDT)
      testSamp <- 1:nrow(inputDT)
    } else if (is.null(testPerc)){ 
      trainSamp <- sample(1:nrow(inputDT), trainPerc*nrow(inputDT))
      testSamp <- createTestSet(inputDT[,.SD,.SDcol = unique(c(unlist(strsplit(unlist(varsGA), '[*]'))))], trainSamp, lengthTestSamp = (nrow(inputDT) - length(trainSamp)))
    } else { 
      trainSamp <- sample(1:nrow(inputDT), trainPerc*nrow(inputDT))
      testSamp <- createTestSet(inputDT[,.SD,.SDcol = unique(c(unlist(strsplit(unlist(varsGA), '[*]'))))], trainSamp, lengthTestSamp = testPerc*nrow(inputDT))
    }
		
    '%notin%' <- Negate('%in%')
    
    # Null model

    formNull <- getForm(distMod, NULL, offset = valOffSet)
    fitNull <- getFitIter(formNull, distMod, inputDT, trainSamp, testSamp, upperBoundPred)
    MSENull <- sum((fitNull$predModel - inputDT$observed[testSamp])^2*inputDT$exposure[testSamp], na.rm = TRUE)/sum(inputDT$exposure[testSamp], na.rm = TRUE)
    inputDT[testSamp, predicted := fitNull$predModel]
      
    #expSplits <- unique(quantile(inputDT[testSamp, exposure], seq(0.01, 0.99, 0.01)))[-1]
    #quantSplits <- seq(0.05, 0.95, 0.05)
    #CPNull <- concProbGrid(inputDT[testSamp, ], lowCat = concProbLow, highCat = concProbHigh, expSplits, quantSplits)$concProbGlobal

	  if(distMod$family[1] %in% c('poisson', 'binomial')){
		  CPNull <- auto_concProb(inputDT, type = 'bin')
	  } else {
		  CPNull <- auto_concProb(inputDT, type = 'cont')
	  }
    
    # Full model
      
    varLevsAll <- extractLevelDT(inputDT[,.SD,.SDcol = c(unique(unlist(strsplit(varsGA, '[*]'))))])
    nLevsAll <- (sum(length(unlist(varLevsAll))) - 1)

    if(nrow(inputDT[trainSamp,])*1.05 > nLevsAll*5){ 
	  
      formFull <- getForm(distMod, varList = (varsGA), offset = NULL)
      fitFull <- getFitIter(formFull, distMod, inputDT, trainSamp, testSamp, upperBoundPred)
      MSEFull <- sum((fitFull$predModel - inputDT$observed[testSamp])^2*inputDT$exposure[testSamp], na.rm = TRUE)/sum(inputDT$exposure[testSamp], na.rm = TRUE)
      inputDT[testSamp, predicted := fitFull$predModel]

      #expSplits <- unique(quantile(inputDT[testSamp, exposure], seq(0.01, 0.99, 0.01)))[-1]
      #quantSplits <- seq(0.05, 0.95, 0.05)
      #CPFull <- concProbGrid(inputDT[testSamp, ], lowCat = concProbLow, highCat = concProbHigh, expSplits, quantSplits)$concProbGlobal
		
	    if(distMod$family[1] %in% c('poisson', 'binomial')){
		    CPFull <- auto_concProb(inputDT, type = 'bin')$concProb 
	    } else {
		    CPFull <- auto_concProb(inputDT, type = 'cont')$concProb 
      }  
      inputDT[, predicted := NULL] 
      
      # MSEFull <- 0.04944892
      # CPFull <- 0.6738922

    } else {
	
	    #In this case, we have more params than obs, so we just choose the theoretical upper bounds  
      CPFull <- 1
      MSEFull <- 0
	  
    } 
    
    MSEFullNew <- MSEFull
    CPFullNew <- CPFull
        
    # Generating an init population
    
    nVar <- length(varsGA)
    
    # random set of models are generated (for init pop)
    randSampInit <- llply(as.list(rep(nVar, nModelsInit)), function(xx){sort(sample(1:xx, nVarInit),decreasing = FALSE)})
    modsInit <- llply(randSampInit, function(xx){varsGA[xx]})
    matBinInit <- matrix(rep(0, nVar*nModelsInit), nrow = nModelsInit)
    for(iRow in 1:nModelsInit){
      matBinInit[iRow, randSampInit[[iRow]]] = rep(1, nVarInit) # On met les 1 ou les variables sont selectionnees, on laisse 0 ailleurs.
    }
    
    #computing some extra vars needed for the first iteration of the GA

    fitInit <- getFit(inputDT, modsInit, nVarMax, nRedMods, trainPerc, testPerc, distMod, valOffSet, FALSE, NULL, NULL, 
      MSEFull, MSENull, MSEFullNew, CPFull, CPNull, CPFullNew, ratioConcProbMSE, upperBoundPred)
    
    concProbInit <- fitInit$concProb; prevInits <- fitInit$intMods
    MSEFullNew <- fitInit$MSEFullNew; MSEFull <- MSEFullNew
    CPFullNew <- fitInit$CPFullNew; CPFull <- CPFullNew
	  firstGen <- 1
	  
	  save.image(paste0('C:/Users/vanoirbe/Desktop/GA/until_init.RData'))
	  #load(paste0('C:/Users/vanoirbe/Desktop/GA/workspace/until_init.RData.RData'))
	
  } else {
  
	  #some sanity checks first
    if(!all(names(statusList) %in% c('matBinInit', 'concProbInit', 'matMutFin', 'matCrossOverFin', 'matNewFitFin', 
                                    'modsIterFin', 'newModsIterFin', 'concProbIterFin', 'concProbWgtsIterFin', 'varsGAFin',
                                    'intModsFin', 'newvalOffSet', 'iGen', 'MSEFull', 'MSENull', 'CPFull', 'CPNull'))){
      stop('statusList doesn t contain all/the right arguments !')
    }
    
    if(!all(nModsGen <= length(statusList$newModsIterFin[[statusList$iGen]]))){
      stop('Elements of nModsGen should be <= last number of models (before being saved).')
    }

	  #defining relevant vars based on statusList
    matBinInit <- statusList$matBinInit
    concProbInit <- statusList$concProbInit
    modsIterFin <- statusList$modsIterFin 
    newModsIterFin <- statusList$newModsIterFin 
    concProbIterFin <- statusList$concProbIterFin 
    concProbWgtsIterFin <- statusList$concProbWgtsIterFin 
    matCrossOverFin <- statusList$matCrossOverFin 
    matMutFin <- statusList$matMutFin 
    matNewFitFin <- statusList$matNewFitFin 
    varsGAFin <- statusList$varsGAFin 
    intModsFin <- statusList$intModsFin 
    MSEFull <- statusList$MSEFull; MSENull <- statusList$MSENull; MSEFullNew <- statusList$MSEFullNew
    CPFull <- statusList$CPFull; CPNull <- statusList$CPNull; CPFullNew <- statusList$CPFullNew
    firstGen <- statusList$iGen + 1
    
    #we need to change the newvalOffSet value inputDT only if such a value is provided in the statusList 
    if(!is.null(statusList$newvalOffSet)){
      inputDT[, newvalOffSet := statusList$newvalOffSet]
    }
	  #since the statusList could only be generated if at least one generation has run, there's no need to generate an init pop
    
  }
  
  # iGen <- 1
  #for(iGen in firstGen:nGens){
  for(iGen in 4:nGens){
    
    cat('iGen :', iGen, '\n')
    procTime <- proc.time()[[3]]
    
    if(iGen == 1){
      
		resGA <- iterGA(inputDT, varsGA, matBinInit, concProbInit, nCrossOverGen[iGen], nMutsGen[iGen], 
						nModsGen[iGen], nVarMax, nRedMods, trainPerc, testPerc, distMod, 
                        iGen, modsInit, nAddedBestModsGen[iGen], valOffSet,
                        nMinInOptModsGen[iGen], includeIntsGen[iGen], nModsIntGen[iGen], nMaxIntGen[iGen], prevInits, 
                        MSEFull, MSENull, MSEFullNew, CPFull, CPNull, CPFullNew, ratioConcProbMSE, upperBoundPred, badOnes)
      
    } else {
      
      resGA <- iterGA(inputDT, varsGAFin[[iGen-1]], matNewFitFin[[iGen-1]], concProbIterFin, nCrossOverGen[iGen], nMutsGen[iGen], 
                      nModsGen[iGen], nVarMax, nRedMods, trainPerc, testPerc, distMod, 
                      iGen, newModsIterFin, nAddedBestModsGen[iGen], valOffSet,
                      nMinInOptModsGen[iGen], includeIntsGen[iGen], nModsIntGen[iGen], nMaxIntGen[iGen], intModsFin, 
                      MSEFull, MSENull, MSEFullNew, CPFull, CPNull, CPFullNew, ratioConcProbMSE, upperBoundPred, badOnes) 
					  
    }
    
    cat('Time iter : ', (proc.time()[[3]] - procTime)/60,'\n')

	  #These vars are reused as input for the next gen in the iterGA function
    matNewFitFin[[iGen]] <- resGA$matNewFit
    concProbIterFin[[iGen]] <- resGA$concProbIter
	  varsGAFin[[iGen]] <- resGA$varsGA
    newModsIterFin[[iGen]] <- resGA$newModsIter
    intModsFin[[iGen]] <- resGA$intMods
	
	  #These vars aren't reused as input for the next gen in the iterGA function
    matMutFin[[iGen]] <- resGA$matMut
    matCrossOverFin[[iGen]] <- resGA$matCrossOver
    modsIterFin[[iGen]] <- resGA$modsIter
    concProbWgtsIterFin[[iGen]] <- resGA$concProbWgtsIter
              
    MSEFullNew <- resGA$MSEFullNew; MSEFull <- MSEFullNew
    CPFullNew <- resGA$CPFullNew; CPFull <- CPFullNew
        
    #intModsFin is empty
    
    if(saveStatus){
      statusList <- list(matBinInit = matBinInit, concProbInit = concProbInit, 
                        matMutFin = matMutFin, matCrossOverFin = matCrossOverFin, matNewFitFin = matNewFitFin, modsIterFin = modsIterFin,
                        newModsIterFin = newModsIterFin, concProbIterFin = concProbIterFin,
                        concProbWgtsIterFin = concProbWgtsIterFin, varsGAFin = varsGAFin, 
                        intModsFin = intModsFin, newvalOffSet = inputDT$newvalOffSet, iGen = iGen, MSEFull = MSEFull, MSENull = MSENull, MSEFullNew = MSEFullNew, CPFull = CPFull, CPNull = CPNull, CPFullNew = CPFullNew) 
      if(is.null(partialSaveName)){
        save(statusList, file = paste0(paste0('statusList_Gen', iGen), '.RData'))
      } else {
        save(statusList, file = paste0(paste0(partialSaveName, iGen), '.RData'))
      }
    }
  }
  
  return(list(matBinInit = matBinInit, concProbInit = concProbInit, 
              matMutFin = matMutFin, matCrossOverFin = matCrossOverFin, matNewFitFin = matNewFitFin, modsIterFin = modsIterFin,
              newModsIterFin = newModsIterFin, concProbIterFin = concProbIterFin,
              concProbWgtsIterFin = concProbWgtsIterFin, varsGAFin = varsGAFin, 
              intModsFin = intModsFin, newvalOffSet = inputDT$newvalOffSet, MSEFull = MSEFull, MSENull = MSENull, CPFull = CPFull, CPNull = CPNull))
  
}

save.image(paste0('C:/Users/vanoirbe/Desktop/GA/test_all.RData'))
# load(paste0('C:/Users/vanoirbe/Desktop/GA/test_all.RData'))
