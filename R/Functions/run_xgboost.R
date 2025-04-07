#' Running function for the XGBoost 
#'
#' @inheritParams run_GA 
#' @param objective objective for `xgboost`
#' @param booster booster for `xgboost`
#' @param ... not used
#' @param nrounds number of rounds in `xgboost`
#' @param params extra parameters for `xgboost`
#' @param nthread number of threads to use for `xgboost`
#' @param perc_variable_selection Percentage of the total gain used for the variable selection (default 90%)
#'
#' @returns list with several values: 
#' "VariableImportance": NULL or named list with the variable importance, 
#' "VariableSubset" = NULL or subset of `variables`, selecting already the best variables 
#' "ConcProbTrainModel" = The Concordance Probability of the Trained Model 
#' "ConcProbTestModel" = The Concordance Probability of the Trained Model with Test data 
#' "ConcProbTrainGLM" = NULL or Concordance Probability of the GLM Model using the selected VariableSubset, using the train data set
#' "ConcProbTestGLM" = NULL or Concordance Probability of the GLM Model using the selected VariableSubset, using the test data set
#' "Model" = NULL or the trained model. 
#' "AdditionalInfo" = NULL or some additional information about the run
#' @export
run_xgboost <- function(train, 
                        test, 
                        variables, 
                        target_variable, 
                        distribution_model, 
                        objective = "count:poisson",
                        booster = "gbtree", 
                        perc_variable_selection = 0.9, 
                        ..., 
                        nrounds = 100, 
                        params = list(), 
                        nthread = 4, 
                        order = 1,
                        offset = "exposure", 
                        concProb_type = "bin", 
                        nu = 100, 
                        withMain = TRUE, 
                        location_glm_results = NULL) {

  # Construction of the Variable Handler for the variables
  VH <- VariableHandler$new(variables = variables, 
                            order = order)
  
  # convert the columns into numeric values for the variable importance 
  train[, (variables) := lapply(.SD, as.numeric), 
        .SDcols = variables]
  test[, (variables) := lapply(.SD, as.numeric), 
       .SDcols = variables]
  
  # get the target values 
  y <- train[[target_variable]]
  
  if (!is.null(offset)) {
    offset_model_train <- train[[offset]]
    offset_model_test <- train[[offset]]
  } else {
    offset_model_train <- NULL
    offset_model_test <- NULL
  }
  
  # Construction of the sparse matrix 
  sparse_matrix_train <- sparse.model.matrix(VH$get_formula(coding = rep(1, VH$get_length()), 
                                                            distMod = distribution_model, 
                                                            targetVar = target_variable), 
                                             data = train)[,-1]
  sparse_matrix_test <- sparse.model.matrix(VH$get_formula(coding = rep(1, VH$get_length()), 
                                                           distMod = distribution_model, 
                                                           targetVar = target_variable), 
                                            data = test)[,-1]
  
  # train the xgboost model
  trained_model <- xgboost(data = sparse_matrix_train, 
                           label = y,
                           nthread = nthread, 
                           objective = objective, 
                           nrounds = nrounds, 
                           booster = booster, 
                           params = params)
  
  # get the results 
  results_output <- list("VariableImportance" = NULL, 
                         "VariableSubset" = NULL, 
                         "ConcProbTrainModel" = NULL, 
                         "ConcProbTestModel" = NULL, 
                         "ConcProbTrainGLM" = NULL, 
                         "ConcProbTestGLM" = NULL, 
                         "Model" = NULL)
  
  ### Variable Importance ####
  var_importance_xgb <- xgb.importance(colnames(sparse_matrix_train), 
                                       model = trained_model)
  
  importance <- var_importance_xgb[["Gain"]]
  names(importance) <-  var_importance_xgb[["Feature"]]
  
  results_output["VariableImportance"] <- list(importance)
  
  ### Variable Subset ####
  # calculate the cumulative result
  var_importance_xgb[, CumGain := cumsum(Gain)]
  # new threshold; just after perc_variable_selection
  threshold <- var_importance_xgb[CumGain >= perc_variable_selection][["CumGain"]][1]
  
  variables_selected <- var_importance_xgb[CumGain <= threshold][["Feature"]]
  results_output[["VariableSubset"]] <- variables_selected
  ### ConcProbTrainModel ####

  predModel_train <- predict(trained_model, 
                             newdata = sparse_matrix_train)
  
  predModel_test <- predict(trained_model, 
                            newdata = sparse_matrix_test)
  
  if(concProb_type == "bin"){
    ConcProbTrainModel <- concProb_bin_fast(train[[target_variable]], 
                                            predModel_train)$concProb
    ConcProbTestModel <- concProb_bin_fast(test[[target_variable]], 
                                           predModel_test)$concProb
  } else if(concProb_type == "cont"){
    ConcProbTrainModel <- concProb_cont_fast(train[[target_variable]], 
                                             predModel_train, 
                                             nu = nu)$concProb
    ConcProbTestModel <- concProb_cont_fast(test[[target_variable]], 
                                            predModel_test, 
                                            nu = nu)$concProb
  } else{
    # Normally is problem already catched at the beginning
    stop(sprintf("The value for 'concProb_type' should be 'bin' or 'cont' not %s", 
                 concProb_type))
  }
  
  results_output["ConcProbTrainModel"] <- ConcProbTrainModel
  results_output["ConcProbTestModel"] <- ConcProbTestModel
  
  # GLM Variable Modelling --------------------------------------------------
  
  # get the results when running the 'standard' GLM 
  results_glm <- retrieve_or_calculate_glm(train = train, 
                                           test = test, 
                                           VH = VH, 
                                           variables = variables_selected, 
                                           target_variable = target_variable, 
                                           distribution_model = distribution_model, 
                                           offset = offset, 
                                           concProb_type = concProb_type, 
                                           nu = nu, 
                                           location_data = location_glm_results, 
                                           withMain = withMain)
  
  results_output["ConcProbTrainGLM"] <- results_glm["ConcProbTrainGLM"]
  results_output["ConcProbTestGLM"] <- results_glm["ConcProbTestGLM"]
  
  ### Model ####
  results_output["Model"] <- list(trained_model)
  
  return(results_output)
}