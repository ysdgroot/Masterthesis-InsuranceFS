#' Title
#'
#' @param matBin 
#' @param nSplit 
#'
#' @return
#' @export
#'
#' @examples
getCrossOver <- function(matBin, nSplit){
  
  nVar <- ncol(matBin)
  nStartMods <- nrow(matBin)
  
  #getting random splits for each couple of models
  sampledWgts <- matBin
  splits <- llply(as.list(rep(nVar, nStartMods/2)), 
                  function(xx){sort(sample(1:xx, nSplit))})
  
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