

#addInt can be NULL and then interaction terms are never taken into account, but if no interaction terms are selected, it is equal to FALSE and the fit function without interaction terms is applied.

iterGA <- function(inputDT, varsGA, matFit, concProb, nCrossOver, nMuts, nMods, nVarMax, nRedMods, trainPerc, testPerc, 
                   distMod, iGen, prevMods, nAddedBestMods, valOffSet,
                   nMinInOptMods, addInt, nModsInt, nMaxInt, prevInts,
                   MSEFull, MSENull, MSEFullNew, CPFull, CPNull, CPFullNew, ratioConcProbMSE, upperBoundPred, badOnes = NULL){
  
  
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
  fullFit <- getFit(inputDT, mods, nVarMax, nRedMods, trainPerc, testPerc, distMod, valOffSet, addInt, nModsInt, nMaxInt, 
					MSEFull, MSENull, MSEFullNew, CPFull, CPNull, CPFullNew, ratioConcProbMSE, upperBoundPred, badOnes)
    
  if(iGen == 1){
		
	#the models of the init phase are not considered, hence no comparison with the concProb of models of previous iterations can be made.
    nVar <- length(varsGA)
    matNewFit <- matrix(rep(0,nMods*nVar), nrow = nMods)
    for (iRow in 1:nMods){
      matNewFit[iRow, which(varsGA %in% fullFit$newMods[[iRow]])] = rep(1, length(fullFit$newMods[[iRow]]))
    }
    return(list(concProbIter = fullFit$concProb, matMut = matMut, matCrossOver = resCrossOver$matCrossOver, matNewFit = matNewFit, modsIter = mods, 
		newModsIter = fullFit$newMods, concProbWgtsIter = resReprod$concProbWgts, varsGA = varsGA, intMods = fullFit$intMods, MSEFullNew = MSEFullNew, CPFullNew = CPFullNew))
    
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
	
      addedBestModsTemp <- list(); concProbAddedBestModelsTemp <- list(); addedBestModsIntsTemp <- list()
      addedBestModsTemp[[1]] <- addedBestMods[[1]]; concProbAddedBestModelsTemp[[1]] <- concProbAddedBestModels[[1]]; addedBestModsIntsTemp[[1]] <- addedBestModsInts[[1]]  
      counter <- 1
      
      for(iMod in 1:length(addedBestMods)){
        counterEqual <- 0
        for(jMod in 1:length(addedBestModsTemp)){
          if(length(addedBestMods[[iMod]]) == length(addedBestModsTemp[[jMod]]) && all(addedBestMods[[iMod]] %in% addedBestModsTemp[[jMod]])){
            counterEqual <- counterEqual + 1
          }
        }
        if(counterEqual == 0){
          counter <- counter + 1
          addedBestModsTemp[[counter]] <- addedBestMods[[iMod]]; concProbAddedBestModelsTemp[[counter]] <- concProbAddedBestModels[[iMod]]; addedBestModsIntsTemp[[counter]] <- addedBestModsInts[[iMod]]
        }
      }
      addedBestMods <- addedBestModsTemp; concProbAddedBestModels <- concProbAddedBestModelsTemp; addedBestModsInts <- addedBestModsIntsTemp
    }
    
    # take a random sample (of size nAddedBestMods) from the addedBestMods
    
    if(length(addedBestMods) > 0){
      selInds <- sort(sample(1:length(addedBestMods), min(nAddedBestMods, length(addedBestMods))))
      addedBestMods <- addedBestMods[selInds]; concProbAddedBestModels <- concProbAddedBestModels[selInds]; addedBestModsInts <- addedBestModsInts[selInds]
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
        selIndsAll <- sort(sample((1:(nMods + length(addedBestMods)))[probs > 0], pmin(nMods, length(probs[probs > 0])), replace = replaceBool, prob = probs[probs > 0]/sum(probs[probs > 0])))
      } else {
        selIndsAll <- NULL
      }  
    } else { 
      selIndsAll <- 1:nMods
    }
    
    listFinalModels <- list(); length(listFinalModels) <- nMods; llply(listFinalModels, function(xx) xx <- NA)
    concProbFinalModels <- list(); length(concProbFinalModels) <- nMods; llply(concProbFinalModels, function(xx) xx <- NA)
    intsFinMod <- list(); length(intsFinMod) <- nMods; llply(intsFinMod, function(xx) xx <- NA)
    
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
    
    return(list(concProbIter = concProbFinalModels, matMut = matMut, matCrossOver = resCrossOver$matCrossOver, matNewFit = matNewFit, modsIter = mods, 
		newModsIter = listFinalModels, concProbWgtsIter = resReprod$concProbWgts, varsGA = varsGA, intMods = intsFinMod, MSEFullNew = MSEFullNew, CPFullNew = CPFullNew))
    
  }
}

getReprod <- function(matBin, concProb, minValWgt){
  
  nVar <- ncol(matBin)
  nStartMods <- nrow(matBin)
  
  # reshuffle of the variable position on the model string (as to avoid systematic biases over different iterations)
  
  varSamp <- sample(1:nVar, nVar)
  matBin <- matBin[, varSamp]
  
  # computation of the weights for the reproduction phase based on the concProb
	
  maxConcProb <- sapply(unlist(concProb), function(xx){max(xx, minValWgt)})
  denomWgt <- sum(maxConcProb - minValWgt)
  wgts <- sapply((maxConcProb - minValWgt)/denomWgt, function(xx){max(xx,0)}) 
  
  # reproduction step
  selIndsAll <- sample(1:nStartMods, nStartMods, replace = TRUE, prob = wgts)
  
  # if a given model influences the convergence too much, this can be attempted
  # selIndsAll <- order(wgts, decreasing = TRUE)
  
  return(list(matReprod = matBin[selIndsAll,], concProbWgts = as.list(wgts), varSamp = varSamp))

}
    
getCrossOver <- function(matBin, nSplit){
  
  nVar <- ncol(matBin)
  nStartMods <- nrow(matBin)
  
  #getting random splits for each couple of models
  sampledWgts <- matBin
  splits <- llply(as.list(rep(nVar, nStartMods/2)), function(xx){sort(sample(1:xx, nSplit))})
  
  matCrossOver <- matrix(rep(0, nStartMods*nVar), ncol = nVar)
  for(iMod in 0:((nStartMods/2) - 1)){
    currSplit <- unique(c(0, splits[[iMod+1]], nVar))
    nInts <- length(currSplit) - 1
    for(iInt in 1:nInts){
      lowBound <- currSplit[iInt] # the lower bound is not needed
      upBound <- currSplit[iInt + 1]
      varInd <- seq(lowBound + 1, upBound, 1) # we obtain an interval like ]0,1] and take all the splitting points in between
      if(iInt%%2 == 0){ # the intervals of 2 consecutives rows are interchanged when iInt is even
        matCrossOver[2*iMod + 1, varInd] <- sampledWgts[2*iMod + 2, varInd]
        matCrossOver[2*iMod + 2, varInd] <- sampledWgts[2*iMod + 1, varInd]
      } else { # no swapping is done when iInt is odd
        matCrossOver[2*iMod + 1, varInd] <- sampledWgts[2*iMod + 1, varInd]
        matCrossOver[2*iMod + 2, varInd] <- sampledWgts[2*iMod + 2, varInd]
      }
    }
  }
  return(list(matCrossOver = matCrossOver))
}

getMut <- function(mat, nMut){
  nStartMods <- nrow(mat)
  nVar <- ncol(mat)
  for(iRow in 1:nStartMods){
    rowSampPos <- sort(sample(1:nVar, nMut))
    mat[iRow, rowSampPos] = abs(mat[iRow, rowSampPos] - 1)
  }
  return(mat)
}

getFitMain <- function(inputDT, mods, nVarMax, nRedMods, trainPerc, testPerc, distMod, valOffSet, MSEFull, MSENull, MSEFullNew, CPFull, CPNull, CPFullNew, ratioConcProbMSE, upperBoundPred){
  
  # defining test and training set
  if(trainPerc == 1){
    trainSamp <- 1:nrow(inputDT)
    testSamp <- 1:nrow(inputDT)
  } else if (is.null(testPerc)){ 
    trainSamp <- sample(1:nrow(inputDT), trainPerc*nrow(inputDT))
    testSamp <- createTestSet(inputDT[,.SD,.SDcol = unique(c(unlist(strsplit(unlist(mods), '[*]'))))], trainSamp, lengthTestSamp = (nrow(inputDT) - length(trainSamp)))
  } else { 
    trainSamp <- sample(1:nrow(inputDT), trainPerc*nrow(inputDT))
    testSamp <- createTestSet(inputDT[,.SD,.SDcol = unique(c(unlist(strsplit(unlist(mods), '[*]'))))], trainSamp, lengthTestSamp = testPerc*nrow(inputDT))
  }
  
  # defining some vars that will be used later on
  nStartMods <- length(mods)
  nVarsMods <- llply(mods, function(xx){length(xx)})
  modsUpPos <- which(nVarsMods > nVarMax); modsUp <- mods[modsUpPos]
  modsDownPos <- which(nVarsMods <= nVarMax); modsDown <- mods[modsDownPos]
  
  # fitting, predicting and computing the concProb for those models with not too much vars (<= nVarMax)
  if(length(modsDown) > 0){
    
    concProb_res <- rep(NA, length(modsDown))
    for(iModDown in 1:length(modsDown)){
      
      form <- getForm(distMod, unique(c(modsDown[[iModDown]], unlist(strsplit(modsDown[[iModDown]], '[*]')))), offset = valOffSet)
      predModelDown <- getFitIter(form, distMod, inputDT, trainSamp, testSamp, upperBoundPred)$predModel
	    tempDT <- inputDT[testSamp,.SD,.SDcol = which(names(inputDT) %in% c('obs', 'exposure'))][, predicted := predModelDown]
      temp <- evalMod(distMod, tempDT, MSEFull = MSEFull, MSENull = MSENull, MSEFullNew = MSEFullNew, CPFull = CPFull, CPNull = CPNull, CPFullNew = CPFullNew, ratioConcProbMSE = ratioConcProbMSE)
	  
      concProb_res[iModDown] <- temp$res
      if(temp$MSEFullNew < MSEFullNew) MSEFullNew <- temp$MSEFullNew 
      if(temp$CPFullNew > CPFullNew) CPFullNew <- temp$CPFullNew
    }
    concProbDown <- concProb_res
  }
  
  # Fitting, predicting and computing the concProb for those models with too much vars (> nVarMax), hence at random the number of vars 
  # that are too much are removed, which is repeated nRedMods times. The best randomly selected model is then kept.
  
  if(length(modsUp) > 0){
    
    newModsUp <- list()
    concProbUp <- list()
    
    for(iMod in 1:length(modsUp)){
          
      currMod = modsUp[[iMod]]
      currMods = llply(as.list(rep(length(currMod), nRedMods)),function(xx){currMod[sort(sample(1:xx, nVarMax))]})
      concProb_res <- rep(NA, length(currMods))
      
      for(iCurrMod in 1:nRedMods){
        
        form <- getForm(distMod, currMods[[iCurrMod]], offset = valOffSet)
        predcurrMod <- getFitIter(form, distMod, inputDT, trainSamp, testSamp, upperBoundPred)$predModel
		    tempDT <- inputDT[testSamp,.SD,.SDcol = which(names(inputDT) %in% c('obs', 'exposure'))][, predicted := predcurrMod]
        temp <- evalMod(distMod, tempDT, MSEFull = MSEFull, MSENull = MSENull, MSEFullNew = MSEFullNew, CPFull = CPFull, CPNull = CPNull, CPFullNew = CPFullNew, ratioConcProbMSE = ratioConcProbMSE)
		
        concProb_res[iCurrMod] <- temp$res
        if(temp$MSEFullNew < MSEFullNew) MSEFullNew <- temp$MSEFullNew 
        if(temp$CPFullNew > CPFullNew) CPFullNew <- temp$CPFullNew
      }
	    # only the best out of nRedMods models is kept
      newModsUp[[iMod]] <- currMods[[which.max(concProb_res)]]
      concProbUp[[iMod]] <- max(concProb_res)
    }
  }
  
  # output objects are constructed
  concProb <- list(); length(concProb) <- nStartMods; llply(concProb, function(xx) xx <- NA)
  newMods <- list(); length(newMods) <- nStartMods; llply(newMods, function(xx) xx <- NA)
  if(length(modsDown) > 0){
    concProb[modsDownPos] <- concProbDown
    newMods[modsDownPos] <- modsDown 
  }
  if(length(modsUp) > 0){
    concProb[modsUpPos] <- concProbUp
    newMods[modsUpPos] <- newModsUp
  }
  
  return(list(concProb = concProb, newMods = newMods, intMods = as.list(rep(NA, length(newMods))), MSEFullNew = MSEFullNew, CPFullNew = CPFullNew))
  
}

# difference with getFitMain is that an additional constraint is added for the interaction terms: we allow for at most nMaxInt interaction terms 
# and we create at random nModsInt models to select the optimal set of interaction terms.

#badOnes: these are the sets of interaction terms of which we have seen that they are useless and hence they can be discarded from the complete set of interaction terms.

getFitInt <- function(inputDT, mods, nVarMax, nRedMods, trainPerc, testPerc, distMod, valOffSet, nModsInt, nMaxInt, MSEFull, MSENull, CPFull, CPNull, ratioConcProbMSE, upperBoundPred, badOnes){
  
# defining test and training set
  if(trainPerc == 1){
    trainSamp <- 1:nrow(inputDT)
    testSamp <- 1:nrow(inputDT)
  } else if (is.null(testPerc)){ 
    trainSamp <- sample(1:nrow(inputDT), trainPerc*nrow(inputDT))
    testSamp <- createTestSet(inputDT[,.SD,.SDcol = unique(c(unlist(strsplit(unlist(mods), '[*]'))))], trainSamp, lengthTestSamp = (nrow(inputDT) - length(trainSamp)))
  } else { 
    trainSamp <- sample(1:nrow(inputDT), trainPerc*nrow(inputDT))
    testSamp <- createTestSet(inputDT[,.SD,.SDcol = unique(c(unlist(strsplit(unlist(mods), '[*]'))))], trainSamp, lengthTestSamp = testPerc*nrow(inputDT))
  }
  
  # defining some vars that will be used later on
  nStartMods <- length(mods)
  nVarsMods <- llply(mods, function(xx){length(xx)})
  modsUpPos <- which(nVarsMods > nVarMax); modsUp <- mods[modsUpPos]
  modsDownPos <- which(nVarsMods <= nVarMax); modsDown <- mods[modsDownPos]
  
  # fitting, predicting and computing the concProb for those models with not too much vars (<= nVarMax)
  if(length(modsDown) > 0){
    
    concProbDown <- list(); length(concProbDown) <- length(modsDown)
    intModsDown <- list(); length(intModsDown) <- length(modsDown) #list of the optimal interaction terms
    
	# for each model, a random sample of all possible interaction terms is made, and only the best set is retained
    
    for(iMod in 1:length(modsDown)){
      
      currMod <- modsDown[[iMod]]
      allVar <- c(currMod)      
      nInt <- sapply(sample(1:(length(allVar)*(length(allVar)-1)/2), nModsInt, replace = TRUE), 
                              function(xx){min(xx, nMaxInt)})      
      possibleInts <- setdiff(allFirstOrderIntTerms(allVar), badOnes)
      
      nIntTemp <- pmin(nModsInt, length(possibleInts))
      intList <- llply(as.list(nIntTemp), function(xx){sample(possibleInts, xx)})
      
      concProb_res <- rep(NA, length(nIntTemp))
      
      for(iInt in 1:length(nIntTemp)){
        
        form <- getForm(distMod, c(currMod, intList[[iInt]]), offset = valOffSet)
        predModDownInt <- getFitIter(form, distMod, inputDT, trainSamp, testSamp, upperBoundPred)$predModel
		    tempDT <- inputDT[testSamp,.SD,.SDcol = which(names(inputDT) %in% c('obs', 'exposure'))][, predicted := predModDownInt]
        temp <- evalMod(distMod, tempDT, MSEFull = MSEFull, MSENull = MSENull, MSEFullNew = MSEFullNew, CPFull = CPFull, CPNull = CPNull, CPFullNew = CPFullNew, ratioConcProbMSE = ratioConcProbMSE)						  

        concProb_res[iInt] <- temp$res
        if(temp$MSEFullNew < MSEFullNew) MSEFullNew <- temp$MSEFullNew 
        if(temp$CPFullNew > CPFullNew) CPFullNew <- temp$CPFullNew
		
      }  
      # only the best model out of nIntTemp models is taken into account
      concProbDown[[iMod]] <- max(concProb_res) 
      intModsDown[[iMod]] <- intList[[which.max(concProb_res)]]
	  
    }
	
    if(length(intModsDown) < nMaxInt){
      startLen <- length(intModsDown)
      counter <- 1
      while((startLen + counter) <= nMaxInt){
        intModsDown[[startLen + counter]] <- ''
        counter <- counter + 1
      }
    }
	
  }
    
  # Fitting, predicting and computing the concProb for those models with too much vars (> nVarMax), hence at random the number of vars 
  # that are too much are removed, which is repeated nRedMods times. The best randomly selected model is then kept.
  
  # it is also checked if the number of interaction terms is lower than nModsInt
  
  if(length(modsUp) > 0){
    
    newModsUp <- list(); length(newModsUp) <- length(modsUp)
    concProbUp <- list(); length(concProbUp) <- length(modsUp)
    intModsUp <- list(); length(intModsUp) <- length(modsUp) #list of the optimal interaction terms
    
    for(iMod in 1:length(modsUp)){
      
      subMod <- modsUp[[iMod]]
      subMods <- llply(as.list(rep(length(subMod), nRedMods)), function(xx){subMod[sort(sample(1:xx, nVarMax))]})    
      concProbSubMod <- list(); length(concProbSubMod) <- nRedMods
      intSubMod <- list(); length(intSubMod) <- nRedMods
      
	    # for each model, a random sample of all possible interaction terms is made, and only the best set is retained
      for(iSub in 1:length(subMods)){
        
        currMod <- subMods[[iSub]]
        allVar <- c(currMod)        
        nInt <- sapply(sample(1:(length(allVar)*(length(allVar)-1)/2), nModsInt, replace = TRUE), 
                                function(xx){min(xx, nMaxInt)})
								
        possibleInts <- setdiff(allFirstOrderIntTerms(allVar), badOnes)
        nIntTemp <- pmin(nModsInt, length(possibleInts))
        intList <- llply(as.list(nIntTemp), function(xx){sample(possibleInts, xx)})
        concProb_res <- rep(NA, length(nIntTemp))
        
        for(iInt in 1:length(nIntTemp)){
          
          form <- getForm(distMod, c(currMod, intList[[iInt]]), offset = valOffSet)
          predcurrModUpInt <- getFitIter(form, distMode, inputDT, trainSamp, testSamp, upperBoundPred)$predModel
		      tempDT <- inputDT[testSamp,.SD,.SDcol = which(names(inputDT) %in% c('obs', 'exposure'))][, predicted := predcurrModUpInt]
          temp <- evalMod(distMod, tempDT, MSEFull = MSEFull, MSENull = MSENull, MSEFullNew = MSEFullNew, CPFull = CPFull, CPNull = CPNull, CPFullNew = CPFullNew, ratioConcProbMSE = ratioConcProbMSE)

          concProb_res[iInt] <- temp$res
          if(temp$MSEFullNew < MSEFullNew) MSEFullNew <- temp$MSEFullNew 
          if(temp$CPFullNew > CPFullNew) CPFullNew <- temp$CPFullNew          
        }    
        
        concProbSubMod[[iSub]] <- max(concProb_res)
        intSubMod[[iSub]] <- intList[[which.max(concProb_res)]]  
      }
      
      bestSubModPos <- which.max(unlist(concProbSubMod)) 
      newModsUp[[iMod]] <- subMods[[bestSubModPos]]
      concProbUp[[iMod]] <- max(unlist(concProbSubMod))
      intModsUp[[iMod]] <- intSubMod[[bestSubModPos]] 
    }
  }
  
  concProb <- list(); length(concProb) <- nStartMods; llply(concProb, function(xx) xx <- NA)
  newMods <- list(); length(newMods) <- nStartMods; llply(newMods, function(xx) xx <- NA)
  intMods <- list(); length(intMods) <- nStartMods; llply(intMods, function(xx) xx <- NA)
  
  if(length(modsDown) > 0){
    concProb[modsDownPos] <- concProbDown
    newMods[modsDownPos] <- modsDown
    intMods[modsDownPos] <- intModsDown
  }
  if(length(modsUp) > 0){
    concProb[modsUpPos] <- concProbUp
    newMods[modsUpPos] <- newModsUp
    intMods[modsUpPos] <- intModsUp    
  }  
  return(list(concProb = concProb, newMods = newMods, intMods = intMods, MSEFullNew = MSEFullNew, CPFullNew = CPFullNew))
}

#fitting the mods, extracting the predictions and computing the concProb

# mods = modsInit
# nRedMods = nRedModels
# addInt = FALSE
# nModsInt = NULL
# nMaxInt = NULL
# 
# nRedModels, trainPerc, testPerc, distMod, valOffSet, FALSE, NULL, NULL, MSEFull, MSENull, MSEFullNew, CPFull, CPNull, CPFullNew, ratioConcProbMSE, upperBoundPred

getFit <- function(inputDT, mods, nVarMax, nRedMods, trainPerc, testPerc, distMod, valOffSet, addInt, nModsInt, nMaxInt, 
				   MSEFull, MSENull, MSEFullNew, CPFull, CPNull, CPFullNew, ratioConcProbMSE, upperBoundPred, badOnes = NULL){
  
  if(is.null(addInt) || (addInt == FALSE)){
    currFit <- getFitMain(inputDT, mods, nVarMax, nRedMods, trainPerc, testPerc, distMod, valOffSet, 
                           MSEFull, MSENull, MSEFullNew, CPFull, CPNull, CPFullNew, ratioConcProbMSE, upperBoundPred)
  } else {
    currFit <- getFitInt(inputDT, mods, nVarMax, nRedMods, trainPerc, testPerc, distMod, valOffSet, nModsInt, nMaxInt, 
						 MSEFull, MSENull, MSEFullNew, CPFull, CPNull, CPFullNew, ratioConcProbMSE, upperBoundPred, badOnes)
  }
  return(currFit)
}

# varList = list(varsGA)
# offset = NULL

getForm <- function(distMod, varList, offset = NULL){
  
  if(distMod$family[1] == 'poisson'){
    targetVar = 'claimNumber'
    linkFunc = distMod$link
  } else if (distMod$family[1] %in% c('Gamma', 'gaussian')){
    targetVar = 'claimSize'
    linkFunc = distMod$link
  } else if (distMod$family[1] == 'binomial'){
    targetVar = 'as.factor(claimSizeProb)'
    linkFunc = distMod$link
  }
  
  if(is.null(offset)){
    form <- paste(targetVar, '~ ', sep = "")
  } else {
    form <- paste(targetVar, '~ ', ' offset(', linkFunc, '(', offset, ')', ')', sep = "")
  }
 
  if(length(unlist(varList)) > 0){ 
    for(iList in 1:length(varList)){
      if(length(varList[[iList]] == 1)){
		form <- paste(form, ' + ', varList[[iList]], sep = "")
      } else {
        form <- paste(form, ' + ', paste(varList[[iList]], collapse = " + "), sep = "")
      }
    }
  } else { #only intercept + offset
    form <- paste(form, ' + 1', sep = "")
  }
  
  form <- as.formula(form)
  return(form)
}

#this function returns the fit of the model on the training set and the predictions on the test set.

#form <- formNull
#form <- formFull

getFitIter <- function(form, distMod, inputDT, trainSamp, testSamp, upperBoundPred){
  
  if(distMod$family[1] == 'poisson'){ 
    if(form == 'claimNumber ~ offset(log(exposure)) + 1'){
      fitModel <- speedglm(formula = form, family = distMod, data = inputDT[trainSamp,])
    } else {
      #fitModel <- glm(formula = form, family = distMod, data = inputDT[trainSamp,])  
      fitModel <- bam(formula = form, family = distMod, data = inputDT[trainSamp,], chunk.size = min(5000, nrow(inputDT)))
      #fitModel <- bam(formula = claimNumber ~ offset(log(exposure)) + nYears + age * carCat + density * carVal * nYears + carVal * cover  + carType * cover, family = distMod, data = inputDT[trainSamp,], chunk.size = min(5000, nrow(inputDT)))
    }
  } else { #weight for claimNumber needs to be added here
    #fitModel <- glm(formula = form, family = distMod, data = inputDT[trainSamp,], weights = claimNumber)  
    fitModel <- bam(formula = form, family = distMod, data = inputDT[trainSamp,], chunk.size = min(5000, nrow(inputDT)), weights = claimNumber)
    
  }
	
  predModel <- predict(fitModel, inputDT[testSamp,], type = 'response')
  if(!is.null(upperBoundPred)){
    predModel <- sapply(predModel, function(xx) min(xx, upperBoundPred))
  }
  return(list(fitModel = fitModel, predModel = as.vector(predModel)))
}

createTestSet <- function(inputDT, sampTrain, lengthTestSamp){
  
  levs <- llply(1:ncol(inputDT), function(xx){unique(inputDT[sampTrain,.SD,.SDcol = xx][[1]])})
  indTestAll <- setdiff(1:nrow(inputDT), sampTrain)
  
  levsOther <- llply(1:ncol(inputDT), function(xx){unique(inputDT[indTestAll, .SD, .SDcol = xx][[1]])})
  
  varIn <- list()
  varInInd <- list()
  countervarIn <- 0
  
  for (iVar in 1:length(levsOther)){
    if(all(levsOther[[iVar]] %in% levs[[iVar]])){
      countervarIn <- countervarIn + 1
      varIn[[countervarIn]] <- names(inputDT[,.SD,.SDcol = iVar])
      varInInd[[countervarIn]] <- iVar
    }
  }
  
  varOutInd <- setdiff(1:ncol(inputDT), unlist(varInInd))
  
  nVarOutInd <- list()
  
  '%notin%' <- Negate('%in%')
  if(length(varOutInd) > 0){
    length(nVarOutInd) <- length(varOutInd)
    for (iVar in 1:length(varOutInd)){
      nVarOutInd[[iVar]] <- indTestAll[which(inputDT[indTestAll, .SD,.SDcol = varOutInd[iVar]][[1]] %notin% vecLevels[[varOutInd[iVar]]])]
    }
    indTestFin <- setdiff(indTestAll, unique(unlist(nVarOutInd)))
  } else {
    indTestFin <- indTestAll
  }
  
  sampTest <- sample(indTestFin, min(lengthTestSamp, length(indTestFin)))
  
  if(lengthTestSamp > length(indTestFin)){
    warning('Less value than specified in lengthTestSamp')
  }
  
  return(sampTest)
}

auto_concProb <- function(inputDT, type = 'bin', nu = 0){
	if(sum(names(inputDT) == 'observed')) setnames(inputDT, 'observed', 'obs')
	if(sum(names(inputDT) == 'predicted')) setnames(inputDT, 'predicted', 'pred')
  if(type == 'cont'){
    res <- concProb_cont_fast(inputDT$obs, inputDT$pred, nu = nu)$concProb
  } else if(type == 'bin'){
    res <- concProb_bin_fast(inputDT$obs, inputDT$pred)$concProb
  }
  return(res)
}

evalMod <- function(distMod, inputDT, MSEFull, MSENull, MSEFullNew, CPFull, CPNull, CPFullNew, ratioConcProbMSE){

  if(distMod$family[1] == 'poisson' || distMod$family[1] == 'binomial'){
    
    if((!is.null(ratioConcProbMSE)) && (ratioConcProbMSE == 0)){
      
	    res <- auto_concProb(inputDT, type = 'bin') 
      
    } else if((!is.null(ratioConcProbMSE)) && (ratioConcProbMSE == 1)){
      
      currMSE <- sum((inputDT$predicted - inputDT$observed)^2*inputDT$exposure, na.rm = TRUE)/sum(inputDT$exposure, na.rm = TRUE)
      res <- -currMSE 
      
    } else {
      
      if(is.null(ratioConcProbMSE)){ratioConcProbMSE <- 0.5}
      
	    currCP <- auto_concProb(inputDT, type = 'bin')
      currMSE <- sum((inputDT$predicted - inputDT$observed)^2*inputDT$exposure, na.rm = TRUE)/sum(inputDT$exposure, na.rm = TRUE)
      res <- max(1 - ((1-ratioConcProbMSE)*((CPFull - currCP)/CPNull) + (ratioConcProbMSE*(currMSE - MSEFull)/MSENull)), 0)
      
      if(!is.na(currCP)){
        if(CPFullNew < currCP){
          CPFullNew <- currCP
        }
      }  
      if(!is.na(currMSE)){
        if(MSEFullNew > currMSE){
          MSEFullNew <- currMSE
        }
      }
    }
    
  } else { # concProb for the continuous case
    
    if((!is.null(ratioConcProbMSE)) && (ratioConcProbMSE == 0)){
      
      currCP <- auto_concProb(inputDT, type = 'cont') 
      res <- currCP
      
    } else if ((!is.null(ratioConcProbMSE)) && (ratioConcProbMSE == 1)){
      
      currMSE <- mse(inputDT$observed, inputDT$predicted, na.rm = TRUE)
      res <- -currMSE
      
    } else {
      
      if(is.null(ratioConcProbMSE)){ratioConcProbMSE <- 0.5}
      
	    currCP <- auto_concProb(inputDT, type = 'cont') 
      currMSE <- mse(inputDT$observed, inputDT$predicted, na.rm = TRUE)
      res <- max(1 - ((1-ratioConcProbMSE)*((CPFull - currCP)/CPNull) + (ratioConcProbMSE*(currMSE - MSEFull)/MSENull)), 0)
      
      if(!is.na(currCP)){
        if(CPFullNew < currCP){
          CPFullNew <- currCP
        }
      }  
      if(!is.na(currMSE)){
        if(MSEFullNew > currMSE){
          MSEFullNew <- currMSE
        }
      }
    }
  }
  
  return(list(res = res, CPFullNew = CPFullNew, MSEFullNew = MSEFullNew))
}
