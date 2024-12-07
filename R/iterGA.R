

#addInt can be NULL and then interaction terms are never taken into account, but if no interaction terms are selected, it is equal to FALSE and the fit function without interaction terms is applied.

#' Go an iteration of the Genetic Algorithm
#'
#' @param inputDT 
#' @param varsGA 
#' @param matFit 
#' @param concProb 
#' @param nCrossOver 
#' @param nMuts 
#' @param nMods 
#' @param nVarMax 
#' @param nRedMods 
#' @param trainPerc 
#' @param testPerc 
#' @param distMod 
#' @param iGen 
#' @param prevMods 
#' @param nAddedBestMods 
#' @param valOffSet 
#' @param nMinInOptMods 
#' @param addInt 
#' @param nModsInt 
#' @param nMaxInt 
#' @param prevInts 
#' @param MSEFull 
#' @param MSENull 
#' @param MSEFullNew 
#' @param CPFull 
#' @param CPNull 
#' @param CPFullNew 
#' @param ratioConcProbMSE 
#' @param upperBoundPred 
#' @param badOnes 
#'
#' @return
#' @export
#'
#' @examples
iterGA <- function(inputDT, 
                   varsGA,
                   matFit, 
                   concProb, 
                   nCrossOver, 
                   nMuts, 
                   nMods, 
                   nVarMax, 
                   nRedMods, 
                   trainPerc, 
                   testPerc,
                   distMod, 
                   iGen, 
                   prevMods, 
                   nAddedBestMods, 
                   valOffSet,
                   nMinInOptMods, 
                   addInt, 
                   nModsInt, 
                   nMaxInt, 
                   prevInts,
                   MSEFull, 
                   MSENull, 
                   MSEFullNew, 
                   CPFull, 
                   CPNull, 
                   CPFullNew, 
                   ratioConcProbMSE, 
                   upperBoundPred, 
                   badOnes = NULL){
  
  
  # the number of models for each iteration can be vary and at the end of the iteration it is decide how many models from 
  # previous iterations are kept, hereby removing the least performant models of the current iteration.

  if(iGen == 1){ #concProb = concProbInit: just simple liste
    orderCP <- order(unlist(concProb), decreasing = TRUE)[1:nMods]
    listCP <- concProb[orderCP]
  } else {
    orderCP <- order(unlist(concProb[[iGen - 1]]), decreasing = TRUE)[1:nMods]
    listCP <- concProb[[iGen - 1]][orderCP]
  }
  matFit <- matFit[orderCP,]
  minValWgt <- min(unlist(listCP), na.rm = TRUE)
  
  print('Reproduction step.')
  resReprod <- getReprod(matFit, listCP, minValWgt)
  
  print('CrossOver step.')
  resCrossOver <- getCrossOver(resReprod$matReprod, nCrossOver)

  print('Mutation step.')
  matMut <- getMut(resCrossOver$matCrossOver, nMuts)
 
  # update of some key vars after the reproduction phase
  varsGA <- varsGA[resReprod$varSamp]
  mods <- list()
  for(iMod in 1:nMods){
    mods[[iMod]] <- varsGA[matMut[iMod,] == 1]
  }
  
  #refitting the sampled models and extracting the concProb
  print('Fit, predict, concProb steps.')
  fullFit <- getFit(inputDT, 
                    mods, 
                    nVarMax, 
                    nRedMods, 
                    trainPerc, 
                    testPerc, 
                    distMod, 
                    valOffSet, 
                    addInt,
                    nModsInt, 
                    nMaxInt, 
					          MSEFull, 
					          MSENull, 
					          MSEFullNew, 
					          CPFull, 
					          CPNull, 
					          CPFullNew, 
					          ratioConcProbMSE, 
					          upperBoundPred, 
					          badOnes)
    
  if(iGen == 1){
		
	#the models of the init phase are not considered, hence no comparison with the concProb of models of previous iterations can be made.
    nVar <- length(varsGA)
    matNewFit <- matrix(rep(0,nMods*nVar), nrow = nMods)
    for (iRow in 1:nMods){
      matNewFit[iRow, which(varsGA %in% fullFit$newMods[[iRow]])] = rep(1, length(fullFit$newMods[[iRow]]))
    }
    return(list(concProbIter = fullFit$concProb, 
                matMut = matMut, 
                matCrossOver = resCrossOver$matCrossOver, 
                matNewFit = matNewFit, 
                modsIter = mods, 
		            newModsIter = fullFit$newMods, 
		            concProbWgtsIter = resReprod$concProbWgts, 
		            varsGA = varsGA, 
		            intMods = fullFit$intMods, 
		            MSEFullNew = MSEFullNew, 
		            CPFullNew = CPFullNew))
    
  } else {
    
    # first we search all the models from the previous iteration that have a concProb higher than the highest concProb of the current iteration (addedBestMods)
	
    highestConcProbCurrIter <- max(unlist(fullFit$concProb), na.rm = TRUE)
    
    addedBestMods <- list()
    concProbAddedBestModels <- list()
    addedBestModsInts <- list() # adding the interaction terms of these best models; this object will remain empty if no interaction terms were found.
    counterOpt <- 0
    for(iGen in 1:length(prevMods)){
      for(iMod in 1:length(prevMods[[iGen]])){
        if(concProb[[iGen]][[iMod]] > highestConcProbCurrIter){
          counterOpt <- counterOpt + 1
          addedBestMods[[counterOpt]] <- prevMods[[iGen]][[iMod]]
          concProbAddedBestModels[[counterOpt]] <- concProb[[iGen]][[iMod]]
          addedBestModsInts[[counterOpt]] <- prevInts[[iGen]][[iMod]]
        }
      }
    }
    
    # only those addedBestMods are retained that are permitted, meaning of which all vars appear in varGA.
    
    whichInvarsGA <- laply(addedBestMods, function(xx){all(xx %in% varsGA)})
    addedBestMods <- addedBestMods[whichInvarsGA]
    concProbAddedBestModels <- concProbAddedBestModels[whichInvarsGA]
    addedBestModsInts <- addedBestModsInts[whichInvarsGA]
    
    # duplicates are removed from addedBestMods
	
    if(length(addedBestMods) > 0){
	
      addedBestModsTemp <- list(); 
      concProbAddedBestModelsTemp <- list()
      addedBestModsIntsTemp <- list()
      addedBestModsTemp[[1]] <- addedBestMods[[1]]
      concProbAddedBestModelsTemp[[1]] <- concProbAddedBestModels[[1]]
      addedBestModsIntsTemp[[1]] <- addedBestModsInts[[1]]  
      counter <- 1
      
      for(iMod in 1:length(addedBestMods)){
        counterEqual <- 0
        for(jMod in 1:length(addedBestModsTemp)){
          if(length(addedBestMods[[iMod]]) == length(addedBestModsTemp[[jMod]]) && 
             all(addedBestMods[[iMod]] %in% addedBestModsTemp[[jMod]])){
            counterEqual <- counterEqual + 1
          }
        }
        if(counterEqual == 0){
          counter <- counter + 1
          addedBestModsTemp[[counter]] <- addedBestMods[[iMod]]
          concProbAddedBestModelsTemp[[counter]] <- concProbAddedBestModels[[iMod]]
          addedBestModsIntsTemp[[counter]] <- addedBestModsInts[[iMod]]
        }
      }
      addedBestMods <- addedBestModsTemp 
      concProbAddedBestModels <- concProbAddedBestModelsTemp
      addedBestModsInts <- addedBestModsIntsTemp
    }
    
    # take a random sample (of size nAddedBestMods) from the addedBestMods
    
    if(length(addedBestMods) > 0){
      selInds <- sort(sample(1:length(addedBestMods), min(nAddedBestMods, length(addedBestMods))))
      addedBestMods <- addedBestMods[selInds]
      concProbAddedBestModels <- concProbAddedBestModels[selInds] 
      addedBestModsInts <- addedBestModsInts[selInds]
    }
    
    # take a random sample (of size nMods) from the models of the current iteration and nAddedBestMods.
    
    if(length(concProbAddedBestModels) > 0){
      probs <- (unlist(c(fullFit$concProb, concProbAddedBestModels)) - min(unlist(fullFit$concProb)))
      #print(paste('probs :', probs)); print(probs[probs > 0]/sum(probs[probs > 0])); print((1:(nMods + length(addedBestMods)))[probs > 0])
      if(length(probs[probs > 0]) > 0){ 
        replaceBool <- FALSE
        if(length(probs[probs > 0]) < nMods){
          replaceBool <- TRUE
        }
        selIndsAll <- sort(sample((1:(nMods + length(addedBestMods)))[probs > 0], 
                                  pmin(nMods, 
                                       length(probs[probs > 0])), 
                                  replace = replaceBool, 
                                  prob = probs[probs > 0]/sum(probs[probs > 0])))
      } else {
        selIndsAll <- NULL
      }  
    } else { 
      selIndsAll <- 1:nMods
    }
    
    listFinalModels <- list(); 
    length(listFinalModels) <- nMods; 
    llply(listFinalModels, function(xx) xx <- NA)
    
    concProbFinalModels <- list(); 
    length(concProbFinalModels) <- nMods; 
    llply(concProbFinalModels, function(xx) xx <- NA)
    
    intsFinMod <- list(); 
    length(intsFinMod) <- nMods; 
    llply(intsFinMod, function(xx) xx <- NA)
    
    if(length(selIndsAll[selIndsAll <= nMods]) >= 1){
      listFinalModels[seq(1, length(selIndsAll[selIndsAll <= nMods]), 1)] <- fullFit$newMods[selIndsAll[selIndsAll <= nMods]]
      concProbFinalModels[seq(1, length(selIndsAll[selIndsAll <= nMods]), 1)] <- fullFit$concProb[selIndsAll[selIndsAll <= nMods]]
      intsFinMod[seq(1, length(selIndsAll[selIndsAll <= nMods]), 1)] <- fullFit$intMods[selIndsAll[selIndsAll <= nMods]]
    }
    
    if(length(selIndsAll[selIndsAll <= nMods]) < nMods){
      if(length(seq(length(selIndsAll[selIndsAll <= nMods]) + 1, nMods, 1)) != length(selIndsAll[selIndsAll > nMods] - nMods)) stop('this is wrong')
      listFinalModels[seq(length(selIndsAll[selIndsAll <= nMods]) + 1, nMods, 1)] <- addedBestMods[selIndsAll[selIndsAll > nMods] - nMods]
      concProbFinalModels[seq(length(selIndsAll[selIndsAll <= nMods]) + 1, nMods, 1)] <- concProbAddedBestModels[selIndsAll[selIndsAll > nMods] - nMods]
      intsFinMod[seq(length(selIndsAll[selIndsAll <= nMods]) + 1, nMods, 1)] <- addedBestModsInts[selIndsAll[selIndsAll > nMods] - nMods]
    }
    
    # this final list will get modified once more, since only those variables will be retained that appear at least nMinInOptMods times in this final set 
	# of models for this iteration. If this is not the case, the variables are removed from model matrix matNewFit (of which the dimension might get smaller then).
	    
    if(!is.null(nMinInOptMods) && nMinInOptMods > 0){
      newVar <- unlist(c(listFinalModels, addedBestMods)); varList <- list(); counter <- 1
      for(iVar in 1:length(varsGA)){
        if(sum(varsGA[iVar] == newVar) >= nMinInOptMods){
          varList[[counter]] <- varsGA[iVar]
          counter <- counter + 1
        }
      }
      cat('Eliminated vars : ', setdiff(varsGA, unlist(varList)), '\n')
      varsGA <- unlist(varList) 
    }
    
    # constructing the new fit matrix, where the vars are removed that do not comply to the nMinInOptMods constraint 
    nVar <- length(varsGA)
    matNewFit <- matrix(rep(0, nMods * nVar), nrow = nMods)
    for(iRow in 1:nMods){
      matNewFit[iRow, which(varsGA %in% listFinalModels[[iRow]])] = 1 
    }
	
	#we should also update intsFinMod, concProbFinalModels, listFinalModels by first refitting the models that were modified 
    
    return(list(concProbIter = concProbFinalModels, 
                matMut = matMut, 
                matCrossOver = resCrossOver$matCrossOver, 
                matNewFit = matNewFit, 
                modsIter = mods, 
		            newModsIter = listFinalModels, 
		            concProbWgtsIter = resReprod$concProbWgts,
		            varsGA = varsGA, 
		            intMods = intsFinMod, 
		            MSEFullNew = MSEFullNew, 
		            CPFullNew = CPFullNew))
    
  }
}

