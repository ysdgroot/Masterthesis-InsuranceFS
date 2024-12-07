#' Evaluate the results 
#' 
#' @param distMod 
#' @param inputDT 
#' @param MSEFull 
#' @param MSENull 
#' @param MSEFullNew 
#' @param CPFull 
#' @param CPNull 
#' @param CPFullNew 
#' @param ratioConcProbMSE 
#'
#' @return
#' @export
evalMod <- function(distMod, 
                    inputDT,
                    MSEFull,
                    MSENull, 
                    MSEFullNew, 
                    CPFull, 
                    CPNull,
                    CPFullNew,
                    ratioConcProbMSE){
  
  if(distMod$family[1] %in% c('poisson',"binomal")){
    type <- "bin" 
  } else {
    type <- "cont"
  }
  
  # if NULL then take 50% for the ConcProb and MSE
  if(is.null(ratioConcProbMSE)){ratioConcProbMSE <- 0.5}
  
  currCP <- auto_concProb(inputDT, type = type)
  currMSE <- sum((inputDT$predicted - inputDT$observed)^2*inputDT$exposure, na.rm = TRUE)/sum(inputDT$exposure, na.rm = TRUE)
  
    
  if(ratioConcProbMSE == 0){
    res <- currCP
  } else if(ratioConcProbMSE == 1){
    #TODO: why do we take the negative of currMSE? 
    res <- -currMSE 
  } else {
    res <- max(1 - ((1-ratioConcProbMSE) * ((CPFull - currCP)/CPNull) + 
                      (ratioConcProbMSE * (currMSE - MSEFull)/MSENull)), 
               0)
    
    if(!is.na(currCP) && CPFullNew < currCP){
        CPFullNew <- currCP
    }  
    if(!is.na(currMSE) && MSEFullNew > currMSE){
        MSEFullNew <- currMSE
    }
  }
    
  return(list(res = res, 
              CPFullNew = CPFullNew, 
              MSEFullNew = MSEFullNew))
}