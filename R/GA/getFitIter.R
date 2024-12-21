#this function returns the fit of the model on the training set and the predictions on the test set.

#form <- formNull
#form <- formFull

#TODO: have a look at the "fastglm" package 
### Using that package may be changing the format for using "model.matrix" to convert formula to matrix 

#' Returns the fit of the model on the training set and the predictions on the test set.
#'
#' @param form 
#' @param distMod 
#' @param inputDT 
#' @param trainSamp 
#' @param testSamp 
#' @param upperBoundPred 
#'
#' @return
#' @export
getFitIter <- function(form, 
                       distMod, 
                       inputDT, 
                       trainSamp, 
                       testSamp, 
                       upperBoundPred){
  
  if(distMod$family[1] == 'poisson'){ 
    if(form == 'claimNumber ~ offset(log(exposure)) + 1'){
      fitModel <- speedglm(formula = form, 
                           family = distMod, 
                           data = inputDT[trainSamp,])
    } else {
      #fitModel <- glm(formula = form, family = distMod, data = inputDT[trainSamp,])  
      fitModel <- bam(formula = form, 
                      family = distMod, 
                      data = inputDT[trainSamp,], 
                      chunk.size = min(5000, nrow(inputDT)))
      #fitModel <- bam(formula = claimNumber ~ offset(log(exposure)) + nYears + age * carCat + density * carVal * nYears + carVal * cover  + carType * cover, family = distMod, data = inputDT[trainSamp,], chunk.size = min(5000, nrow(inputDT)))
    }
  } else { #weight for claimNumber needs to be added here
    #fitModel <- glm(formula = form, family = distMod, data = inputDT[trainSamp,], weights = claimNumber)  
    fitModel <- bam(formula = form, 
                    family = distMod, 
                    data = inputDT[trainSamp,], 
                    chunk.size = min(5000, nrow(inputDT)), 
                    weights = claimNumber)
    
  }
  
  predModel <- predict(fitModel, inputDT[testSamp,], type = 'response')
  if(!is.null(upperBoundPred)){
    predModel <- sapply(predModel, function(xx) min(xx, upperBoundPred))
  }
  return(list(fitModel = fitModel, 
              predModel = as.vector(predModel)))
}
