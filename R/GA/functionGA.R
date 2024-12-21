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


functionGA <- function(inputDT, 
                       varsGA, 
                       nModsGen, 
                       nGens, 
                       nCrossOverGen, 
                       nMutsGen,
                       nVarInit, 
                       nVarMax, 
                       nRedMods = 5, 
                       trainPerc = 0.8, 
                       testPerc = NULL,
                       distMod = poisson(link='log'), 
                       nAddedBestModsGen = rep(5, nGens), 
                       valOffSet = 'exposure',
                       nMinInOptModsGen = NULL, 
                       includeIntsGen = NULL, 
                       nModsIntGen = NULL, 
                       nMaxIntGen = NULL, 
                       saveStatus = TRUE, 
                       partialSaveName = 'statusList_Gen', 
                       statusList = NULL,
                       typeThreshold = NULL,
                       ratioConcProbMSE = NULL,
                       upperBoundPred = NULL, 
                       badOnes = NULL, 
                       observed =  "claimNumber", 
                       seed = 739){
  
    set.seed(seed)
  
  # Initializing and checks  ------------------------------------------------
  
  nModelsInit <- nModsGen[1]
  
  if(!(distMod$family %in% c("poisson", "Gamma", "gaussian", "binomial"))){
    stop('distMod should be poisson, gamma, gaussian or binomial.')
  }
  
  checkDT(inputDT, observed)
  
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
  
  if(!all(diff(c(nModelsInit,nModsGen)) >= 0)){
    stop('Number of models should be decreasing : nModsGen[i] <= nModsGen[i-1]')
  }
  
  checkLength(list(nAddedBestModsGen), nGens)
  
  #if not enough obs: test and training sample are the same
  varLevs <- extractLevelDT(inputDT[,.SD,.SDcol = c(unique(c(unlist(strsplit(unlist(varsGA), '[*]')))))])
  nMaxVarLevs <- max(unlist(llply(1:1e5, function(xx) sum(length(unlist(varLevs[sample(1:length(c(varsGA)), nVarMax)]))) - 1))*5)
  if(nrow(inputDT)*trainPerc*1.05 < (nMaxVarLevs)){ # * 1.05 to create a small margin
    trainPerc <- 1
    cat('Not enough observations were observed for the number of levels that are considered, such that the test and training set are the same (trainPerc = testPerc = 1). \n') 
  }
  
  ###  Init: populating statusList, defining relevant vars (concProb/MSE null and full model, defining training/test set)
  
  #TODO: create an object "statusList" to have the necessary information 
  
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
      testSamp <- createTestSet(inputDT[,.SD,.SDcol = unique(c(unlist(strsplit(unlist(varsGA), '[*]'))))], 
                                trainSamp, 
                                lengthTestSamp = (nrow(inputDT) - length(trainSamp)))
    } else { 
      trainSamp <- sample(1:nrow(inputDT), trainPerc*nrow(inputDT))
      testSamp <- createTestSet(inputDT[,.SD,.SDcol = unique(c(unlist(strsplit(unlist(varsGA), '[*]'))))], 
                                trainSamp, 
                                lengthTestSamp = testPerc*nrow(inputDT))
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
    
    # setup of the NULL - C-index
    if(distMod$family[1] %in% c('poisson', 'binomial')){
      CPNull <- auto_concProb(inputDT, type = 'bin')
    } else {
      CPNull <- auto_concProb(inputDT, type = 'cont')
    }
    
    # Full model
    
    varLevsAll <- extractLevelDT(inputDT[,.SD,.SDcol = c(unique(unlist(strsplit(varsGA, '[*]'))))])
    nLevsAll <- (sum(length(unlist(varLevsAll))) - 1)
    
    if(nrow(inputDT[trainSamp,])*1.05 > nLevsAll*5){ 
      
      # formFull <- getForm(distMod, varList = (varsGA), offset = NULL)
      # fitFull <- getFitIter(formFull, distMod, inputDT, trainSamp, testSamp, upperBoundPred)
      # MSEFull <- sum((fitFull$predModel - inputDT$observed[testSamp])^2*inputDT$exposure[testSamp], na.rm = TRUE)/sum(inputDT$exposure[testSamp], na.rm = TRUE)
      # inputDT[testSamp, predicted := fitFull$predModel]
      # 
      # #expSplits <- unique(quantile(inputDT[testSamp, exposure], seq(0.01, 0.99, 0.01)))[-1]
      # #quantSplits <- seq(0.05, 0.95, 0.05)
      # #CPFull <- concProbGrid(inputDT[testSamp, ], lowCat = concProbLow, highCat = concProbHigh, expSplits, quantSplits)$concProbGlobal
      # 
      # if(distMod$family[1] %in% c('poisson', 'binomial')){
      #   CPFull <- auto_concProb(inputDT, type = 'bin')$concProb 
      # } else {
      #   CPFull <- auto_concProb(inputDT, type = 'cont')$concProb 
      # }  
      # inputDT[, predicted := NULL] 
      
      MSEFull <- 0.04944892
      CPFull <- 0.6738922
      
    } else {
      
      #In this case, we have more params than obs, so we just choose the theoretical upper bounds  
      CPFull <- 1
      MSEFull <- 0
      
    } 
    #TODO: what is the reason for this code? 
    MSEFullNew <- MSEFull
    CPFullNew <- CPFull
    
    #TODO: remove this code!!
    browser()
    
    # Generating an init population
    
    nVar <- length(varsGA)
    
    # random set of models are generated (for init pop)
    randSampInit <- llply(as.list(rep(nVar, nModelsInit)), function(xx){sort(sample(1:xx, nVarInit),decreasing = FALSE)})
    modsInit <- llply(randSampInit, function(xx){varsGA[xx]})
    matBinInit <- matrix(rep(0, nVar*nModelsInit), nrow = nModelsInit)
    for(iRow in 1:nModelsInit){
      matBinInit[iRow, randSampInit[[iRow]]] = rep(1, nVarInit) # 1 when variable is selected, 0 when not
    }
    
    #TODO: remove code!! 
    browser()
    #computing some extra vars needed for the first iteration of the GA
    
    fitInit <- getFit(inputDT, 
                      modsInit, 
                      nVarMax, 
                      nRedMods, 
                      trainPerc, 
                      testPerc, 
                      distMod, 
                      valOffSet, 
                      FALSE, 
                      NULL, 
                      NULL,
                      MSEFull,
                      MSENull, 
                      MSEFullNew, 
                      CPFull, 
                      CPNull,
                      CPFullNew,
                      ratioConcProbMSE,
                      upperBoundPred)
    
    concProbInit <- fitInit$concProb 
    prevInits <- fitInit$intMods
    MSEFullNew <- fitInit$MSEFullNew 
    MSEFull <- MSEFullNew
    CPFullNew <- fitInit$CPFullNew 
    CPFull <- CPFullNew
    firstGen <- 1
    
    save.image(file.path("Data", "until_init.RData"))
    
  } 
  else {
    # if StatusList is not NULL
    
    #some sanity checks first
    if(!all(names(statusList) %in% c('matBinInit', 'concProbInit', 'matMutFin', 
                                     'matCrossOverFin', 'matNewFitFin', 'modsIterFin', 
                                     'newModsIterFin', 'concProbIterFin', 'concProbWgtsIterFin', 
                                     'varsGAFin', 'intModsFin', 'newvalOffSet', 
                                     'iGen', 'MSEFull', 'MSENull', 'CPFull', 'CPNull'))){
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
    MSEFull <- statusList$MSEFull 
    MSENull <- statusList$MSENull 
    MSEFullNew <- statusList$MSEFullNew
    CPFull <- statusList$CPFull 
    CPNull <- statusList$CPNull
    CPFullNew <- statusList$CPFullNew
    firstGen <- statusList$iGen + 1
    
    #we need to change the newvalOffSet value inputDT only if such a value is provided in the statusList 
    if(!is.null(statusList$newvalOffSet)){
      inputDT[, newvalOffSet := statusList$newvalOffSet]
    }
    #since the statusList could only be generated if at least one generation has run, there's no need to generate an init pop
    
  }
  
  # iGen <- 1
  for(iGen in firstGen:nGens){
    
    cat('iGen :', iGen, '\n')
    procTime <- proc.time()[[3]]
    
    if(iGen == 1){
      
      resGA <- iterGA(inputDT, 
                      varsGA, 
                      matBinInit, 
                      concProbInit, 
                      nCrossOverGen[iGen], 
                      nMutsGen[iGen], 
                      nModsGen[iGen], 
                      nVarMax, 
                      nRedMods, 
                      trainPerc, 
                      testPerc, 
                      distMod, 
                      iGen, 
                      modsInit, 
                      nAddedBestModsGen[iGen], 
                      valOffSet,
                      nMinInOptModsGen[iGen], 
                      includeIntsGen[iGen],
                      nModsIntGen[iGen],
                      nMaxIntGen[iGen], 
                      prevInits, 
                      MSEFull,
                      MSENull, 
                      MSEFullNew, 
                      CPFull, 
                      CPNull, 
                      CPFullNew, 
                      ratioConcProbMSE, 
                      upperBoundPred, 
                      badOnes)
      
    } else {
      
      resGA <- iterGA(inputDT, 
                      varsGAFin[[iGen-1]], 
                      matNewFitFin[[iGen-1]], 
                      concProbIterFin, 
                      nCrossOverGen[iGen], 
                      nMutsGen[iGen], 
                      nModsGen[iGen], 
                      nVarMax, 
                      nRedMods, 
                      trainPerc, 
                      testPerc, 
                      distMod, 
                      iGen, 
                      newModsIterFin, 
                      nAddedBestModsGen[iGen], 
                      valOffSet,
                      nMinInOptModsGen[iGen], 
                      includeIntsGen[iGen], 
                      nModsIntGen[iGen],
                      nMaxIntGen[iGen], 
                      intModsFin, 
                      MSEFull, 
                      MSENull, 
                      MSEFullNew, 
                      CPFull, 
                      CPNull, 
                      CPFullNew, 
                      ratioConcProbMSE, 
                      upperBoundPred,
                      badOnes) 
      
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
      statusList <- list(matBinInit = matBinInit, 
                         concProbInit = concProbInit, 
                         matMutFin = matMutFin, 
                         matCrossOverFin = matCrossOverFin, 
                         matNewFitFin = matNewFitFin, 
                         modsIterFin = modsIterFin,
                         newModsIterFin = newModsIterFin, 
                         concProbIterFin = concProbIterFin,
                         concProbWgtsIterFin = concProbWgtsIterFin, 
                         varsGAFin = varsGAFin, 
                         intModsFin = intModsFin, 
                         newvalOffSet = inputDT$newvalOffSet, 
                         iGen = iGen, 
                         MSEFull = MSEFull, 
                         MSENull = MSENull, 
                         MSEFullNew = MSEFullNew, 
                         CPFull = CPFull, 
                         CPNull = CPNull, 
                         CPFullNew = CPFullNew) 
      
      save(statusList, file = paste0(paste0(partialSaveName, iGen), '.RData'))
    }
  }
  
  return(list(matBinInit = matBinInit, 
              concProbInit = concProbInit, 
              matMutFin = matMutFin, 
              matCrossOverFin = matCrossOverFin, 
              matNewFitFin = matNewFitFin, 
              modsIterFin = modsIterFin,
              newModsIterFin = newModsIterFin, 
              concProbIterFin = concProbIterFin,
              concProbWgtsIterFin = concProbWgtsIterFin, 
              varsGAFin = varsGAFin, 
              intModsFin = intModsFin, 
              newvalOffSet = inputDT$newvalOffSet, 
              MSEFull = MSEFull, 
              MSENull = MSENull, 
              CPFull = CPFull,
              CPNull = CPNull))
  
}