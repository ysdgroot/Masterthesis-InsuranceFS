#' Title
#'
#' @param matBin 
#' @param concProb 
#' @param minValWgt 
#'
#' @return
#' @export
getReprod <- function(matBin, 
                      concProb, 
                      minValWgt){
  
  nVar <- ncol(matBin)
  nStartMods <- nrow(matBin)
  
  # reshuffle of the variable position on the model string (as to avoid systematic biases over different iterations)
  
  varSamp <- sample(1:nVar, nVar)
  matBin <- matBin[, varSamp]
  
  # computation of the weights for the reproduction phase based on the concProb
  
  maxConcProb <- sapply(unlist(concProb), 
                        function(xx){max(xx, minValWgt)})
  denomWgt <- sum(maxConcProb - minValWgt)
  wgts <- sapply((maxConcProb - minValWgt)/denomWgt, 
                 function(xx){max(xx,0)}) 
  
  # reproduction step
  selIndsAll <- sample(1:nStartMods, 
                       nStartMods, 
                       replace = TRUE, 
                       prob = wgts)
  
  # if a given model influences the convergence too much, this can be attempted
  # selIndsAll <- order(wgts, decreasing = TRUE)
  
  return(list(matReprod = matBin[selIndsAll,], 
              concProbWgts = as.list(wgts), 
              varSamp = varSamp))
  
}