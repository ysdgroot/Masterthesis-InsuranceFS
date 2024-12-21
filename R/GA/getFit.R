getFitMain <- function(inputDT, 
                       mods, 
                       nVarMax, 
                       nRedMods, 
                       trainPerc, 
                       testPerc, 
                       distMod, 
                       valOffSet, 
                       MSEFull, 
                       MSENull, 
                       MSEFullNew, 
                       CPFull, 
                       CPNull, 
                       CPFullNew, 
                       ratioConcProbMSE, 
                       upperBoundPred){
  
  # defining test and training set
  if(trainPerc == 1){
    trainSamp <- 1:nrow(inputDT)
    testSamp <- 1:nrow(inputDT)
  } else if (is.null(testPerc)){ 
    trainSamp <- sample(1:nrow(inputDT), trainPerc*nrow(inputDT))
    testSamp <- createTestSet(inputDT[,.SD,
                                      .SDcol = unique(c(unlist(strsplit(unlist(mods), '[*]'))))], 
                              trainSamp, 
                              lengthTestSamp = (nrow(inputDT) - length(trainSamp)))
  } else { 
    trainSamp <- sample(1:nrow(inputDT), trainPerc*nrow(inputDT))
    testSamp <- createTestSet(inputDT[,.SD,
                                      .SDcol = unique(c(unlist(strsplit(unlist(mods), '[*]'))))], 
                              trainSamp, 
                              lengthTestSamp = testPerc*nrow(inputDT))
  }
  
  # defining some vars that will be used later on
  nStartMods <- length(mods)
  nVarsMods <- llply(mods, function(xx){length(xx)})
  modsUpPos <- which(nVarsMods > nVarMax) 
  modsUp <- mods[modsUpPos]
  modsDownPos <- which(nVarsMods <= nVarMax)
  modsDown <- mods[modsDownPos]
  
  # fitting, predicting and computing the concProb for those models with not too much vars (<= nVarMax)
  if(length(modsDown) > 0){
    
    concProb_res <- rep(NA, length(modsDown))
    for(iModDown in 1:length(modsDown)){
      
      form <- getForm(distMod, unique(c(modsDown[[iModDown]], unlist(strsplit(modsDown[[iModDown]], '[*]')))), offset = valOffSet)
      predModelDown <- getFitIter(form, distMod, inputDT, trainSamp, testSamp, upperBoundPred)$predModel
      tempDT <- inputDT[testSamp,
                        .SD,
                        .SDcol = which(names(inputDT) %in% c('obs', 'exposure'))][, predicted := predModelDown]
      temp <- evalMod(distMod, 
                      tempDT, 
                      MSEFull = MSEFull, 
                      MSENull = MSENull, 
                      MSEFullNew = MSEFullNew, 
                      CPFull = CPFull, 
                      CPNull = CPNull, 
                      CPFullNew = CPFullNew, 
                      ratioConcProbMSE = ratioConcProbMSE)
      
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
  
  return(list(concProb = concProb, 
              newMods = newMods, 
              intMods = as.list(rep(NA, length(newMods))), 
              MSEFullNew = MSEFullNew, 
              CPFullNew = CPFullNew))
  
}

# difference with getFitMain is that an additional constraint is added for the interaction terms: we allow for at most nMaxInt interaction terms 
# and we create at random nModsInt models to select the optimal set of interaction terms.

#badOnes: these are the sets of interaction terms of which we have seen that they are useless and hence they can be discarded from the complete set of interaction terms.

getFitInt <- function(inputDT, 
                      mods, 
                      nVarMax, 
                      nRedMods, 
                      trainPerc, 
                      testPerc, 
                      distMod, 
                      valOffSet,
                      nModsInt,
                      nMaxInt,
                      MSEFull, 
                      MSENull, 
                      CPFull,
                      CPNull, 
                      ratioConcProbMSE,
                      upperBoundPred, 
                      badOnes){
  
  # defining test and training set
  if(trainPerc == 1){
    trainSamp <- 1:nrow(inputDT)
    testSamp <- 1:nrow(inputDT)
  } else if (is.null(testPerc)){ 
    trainSamp <- sample(1:nrow(inputDT), trainPerc*nrow(inputDT))
    testSamp <- createTestSet(inputDT[,.SD,.SDcol = unique(c(unlist(strsplit(unlist(mods), '[*]'))))], 
                              trainSamp, 
                              lengthTestSamp = (nrow(inputDT) - length(trainSamp)))
  } else { 
    trainSamp <- sample(1:nrow(inputDT), trainPerc*nrow(inputDT))
    testSamp <- createTestSet(inputDT[,.SD,.SDcol = unique(c(unlist(strsplit(unlist(mods), '[*]'))))], 
                              trainSamp, 
                              lengthTestSamp = testPerc*nrow(inputDT))
  }
  
  # defining some vars that will be used later on
  nStartMods <- length(mods)
  nVarsMods <- llply(mods, function(xx){length(xx)})
  modsUpPos <- which(nVarsMods > nVarMax); modsUp <- mods[modsUpPos]
  modsDownPos <- which(nVarsMods <= nVarMax); modsDown <- mods[modsDownPos]
  
  # fitting, predicting and computing the concProb for those models with not too much vars (<= nVarMax)
  if(length(modsDown) > 0){
    
    concProbDown <- list()
    length(concProbDown) <- length(modsDown)
    intModsDown <- list() 
    length(intModsDown) <- length(modsDown) #list of the optimal interaction terms
    
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
        temp <- evalMod(distMod, 
                        tempDT, 
                        MSEFull = MSEFull, 
                        MSENull = MSENull, 
                        MSEFullNew = MSEFullNew, 
                        CPFull = CPFull, 
                        CPNull = CPNull, 
                        CPFullNew = CPFullNew, 
                        ratioConcProbMSE = ratioConcProbMSE)						  
        
        concProb_res[iInt] <- temp$res
        if(temp$MSEFullNew < MSEFullNew) {MSEFullNew <- temp$MSEFullNew} 
        if(temp$CPFullNew > CPFullNew) {CPFullNew <- temp$CPFullNew}
        
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
  return(list(concProb = concProb, 
              newMods = newMods, 
              intMods = intMods, 
              MSEFullNew = MSEFullNew, 
              CPFullNew = CPFullNew))
}

#fitting the mods, extracting the predictions and computing the concProb

# mods = modsInit
# nRedMods = nRedModels
# addInt = FALSE
# nModsInt = NULL
# nMaxInt = NULL
# 
# nRedModels, trainPerc, testPerc, distMod, valOffSet, FALSE, NULL, NULL, MSEFull, MSENull, MSEFullNew, CPFull, CPNull, CPFullNew, ratioConcProbMSE, upperBoundPred

getFit <- function(inputDT, 
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
                   badOnes = NULL){
  
  if(is.null(addInt) || (addInt == FALSE)){
    currFit <- getFitMain(inputDT, mods, nVarMax, nRedMods, trainPerc, testPerc, distMod, valOffSet, 
                          MSEFull, MSENull, MSEFullNew, CPFull, CPNull, CPFullNew, ratioConcProbMSE, upperBoundPred)
  } else {
    currFit <- getFitInt(inputDT, mods, nVarMax, nRedMods, trainPerc, testPerc, distMod, valOffSet, nModsInt, nMaxInt, 
                         MSEFull, MSENull, MSEFullNew, CPFull, CPNull, CPFullNew, ratioConcProbMSE, upperBoundPred, badOnes)
  }
  return(currFit)
}
