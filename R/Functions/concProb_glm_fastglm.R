#' The evaluation method for the discrete outcomes. 
#' Usually for counting numbers like a Poisson-Model 
#'
#' @param coding binary vector. 
#' The length should be the same as what the `variableHandler` can work with. 
#' @param trainDT,testDT data.table with train/test data. 
#' The train data will be used to create the GLM model, the test data for the final result. 
#' @param distMod family of distribution with a link function. 
#' @param variableHandler variableHandler object. 
#' This variableHandler will transform the binary coding into a formula.
#' @param type "bin" or "cont" 
#' "bin" is for categorical variables 
#' "cont" is for continuous variables 
#' This is only used to calculate the Concordance Probability. 
#' The calculation depends on the type of variable. 
#' @param nu positive integer, 
#' only used when `type` = "cont" for the calculation of the Concordance Probability
#' @param nullValue Constant value to remove from the result. 
#' This is useful for certain algorithms where the value is used to create a probability of being selected for the next part. 
#' By removing a portion the probabilities will be more spread. 
#' Will only be applied to the first result
#' @param offset NULL or character, if NULL then ignored, 
#' if not it will be transformed by the link function and put as offset 
#' @param targetVar character, variable name to be put before the `~`
#' @param withMain logical, 
#' if the main-variables (only for interactions) should be included or not. 
#' Be aware it will only look at the variables for the coding. 
#' So if a main variable is in the fixed or ignored list, it will not be added to the coding.
#' @param message logical, is message if a main variable is not used or not. 
#' Only when `withMain` is set to `TRUE`
#' @param location_save NULL or a folder location. 
#' If it is a folder location, and .RDS file will be searched based on the `coding`.
#' If such file is found it will load and return that value, 
#' Otherwise, the GLM model will be created and will save the value if a folder is given. 
#' @param method variable `method` from the function [fastglm::fastglm()]. 
#' Default is 3 because of the speed performance
#'
#' @returns list with the all the results. 
#' First value 
#' @export
concProb_glm_fastglm <- function(coding, 
                         trainDT, 
                         testDT, 
                         distMod, 
                         variableHandler,
                         targetVar, 
                         nullValue = 0,
                         type = "bin", 
                         nu = 0,
                         offset = NULL, 
                         withMain = TRUE, 
                         message = FALSE, 
                         location_save = NULL, 
                         method = 3){
  
  if (!(type %in% c("bin","cont"))) {
    stop(sprintf("The value for 'type' should be 'bin' or 'cont' not %s", 
                 type))
  }
  
  # first to check
  if (!is.null(location_save)){
    if(!dir.exists(location_save)){
      stop(sprintf("The folder %s does not exist"))
    }
    
    file_location <- file.path(location_save, 
                               sprintf("%s.RDS", 
                                       paste(coding, 
                                             collapse = "")))
    if (file.exists(file_location)) {
      results <- readRDS(file_location)
      return(results)
    }
  }
  
  formula_glm <- variableHandler$getFormula(coding = coding, 
                                            distMod = distMod, 
                                            targetVar = targetVar, 
                                            offset = offset, 
                                            withMain = withMain, 
                                            message = message)
  
  # model matrix for the train data, needed for the construction of the GLM
  mm <- model.matrix(formula_glm, 
                     data = trainDT)
  
  fitModel <- fastglm(x = mm, 
                      y = trainDT[[targetVar]], 
                      family = distMod, 
                      data = trainDT, 
                      method = method)
  
  # model matrix for the test data, needed for the predictions
  mm_test <- model.matrix(formula_glm, 
                     data = testDT)
  
  # Get Concordance Probability of the test data
  predModel_test <- predict(fitModel, 
                            newdat = mm_test, 
                            type = 'response')
  
  # Get Concordance Probability of the train and test data
  if(type == "bin"){
    result_testdata <- concProb_bin_fast(testDT[[targetVar]], 
                                         predModel_test)$concProb
    result_traindata <- concProb_bin_fast(trainDT[[targetVar]], 
                                          fitModel$fitted.values)$concProb
  } else if(type == "cont"){
    result_testdata <- concProb_cont_fast(testDT[[targetVar]], 
                                          predModel_test, 
                                          nu = nu)$concProb
    result_traindata <- concProb_cont_fast(trainDT[[targetVar]], 
                                           fitModel$fitted.values, 
                                           nu = nu)$concProb
  } else{
    # Normally is problem already catched at the beginning
    stop(sprintf("The value for 'type' should be 'bin' or 'cont' not %s", 
                 type))
  }
  
  # remove the NullValue such that the selection is better 
  result <- result_testdata - nullValue
  
  results <- list("Result" = as.numeric(result),
                  "TestData" = as.numeric(result_testdata), 
                  "TrainData" = as.numeric(result_traindata), 
                  "AIC" = AIC(fitModel), 
                  "BIC" = BIC(fitModel))
  
  if (!is.null(location_save)) {
    # save the result if the location is given
    saveRDS(results, 
            file_location)
  }
  
  return(results)
}
