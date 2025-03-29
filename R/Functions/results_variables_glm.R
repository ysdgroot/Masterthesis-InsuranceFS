#' Get results for the trains and test data sets using the Concordance Probability
#'
#' @param train,test data.table with the train and test set
#' @param variables Variables from `train` & `test` used for the training. 
#' This can also be interactions 
#' @param target_variable the target Variable
#' @param distribution_model Distribution model for the GLM
#' @param offset column of the offset, in case there is an offset
#' @param concProb_type "bin" for integers (Concordance Probability) 
#' or "cont" for continuous target variable
#' @param nu threshold for the Concordance Probability, when `type`="cont"
#'
#' @returns list with two values, ConcProbTestGLM and ConcProbTrainGLM. 
#' Each gives the Concordance Probability for the test and train dataset  
#' 
#' @export
results_variables_glm <- function(train, 
                                  test, 
                                  variables, 
                                  target_variable, 
                                  distribution_model, 
                                  offset = NULL, 
                                  concProb_type = "bin", 
                                  nu = 100) {
  
  # first position for the Feature selection
  output_list <- list("ConcProbTestGLM" = NULL, 
                      "ConcProbTrainGLM" = NULL)
  
  ### ConcProbTrainGLM & ConcProbTestGLM ####
  # Construction of the GLM model with the Trained Dataset
  trained_GLM <- get_fastglm_model(train, 
                                   variables = variables, 
                                   target_variable = target_variable, 
                                   distMod = distribution_model, 
                                   offset = offset, 
                                   method = 3)
  # construct the model matrix for the test set
  mm_test <- construct_modelmatrix(test, 
                                   variables = variables, 
                                   target_variable = target_variable, 
                                   distMod = distribution_model, 
                                   offset = offset)
  
  # Get Concordance Probability of the test data
  predModel_test <- predict(trained_GLM, 
                            newdat = mm_test, 
                            type = 'response')
  
  # Get Concordance Probability of the train and test data
  if(concProb_type == "bin"){
    ConcProbTestGLM <- concProb_bin_fast(test[[target_variable]], 
                                         predModel_test)$concProb
    ConcProbTrainGLM <- concProb_bin_fast(train[[target_variable]], 
                                          trained_GLM$fitted.values)$concProb
  } else if(concProb_type == "cont"){
    ConcProbTestGLM <- concProb_cont_fast(test[[target_variable]], 
                                          predModel_test, 
                                          nu = nu)$concProb
    ConcProbTrainGLM <- concProb_cont_fast(train[[target_variable]], 
                                           trained_GLM$fitted.values, 
                                           nu = nu)$concProb
  } else{
    # Normally is problem already catched at the beginning
    stop(sprintf("The value for 'concProb_type' should be 'bin' or 'cont' not %s", 
                 concProb_type))
  }
  
  # case that there is an issue, like glm only has intercept
  if(is.nan(ConcProbTrainGLM)) {
    ConcProbTrainGLM <-  0.5
  }
  if(is.nan(ConcProbTestGLM)) {
    ConcProbTestGLM <-  0.5
  }
  
  output_list["ConcProbTrainGLM"] <- ConcProbTrainGLM
  output_list["ConcProbTestGLM"] <- ConcProbTestGLM
  
  return(output_list)
}