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