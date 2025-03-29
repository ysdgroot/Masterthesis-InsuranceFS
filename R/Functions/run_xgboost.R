#' Title
#'
#' @param train 
#' @param test 
#' @param variables 
#' @param target_variable 
#' @param distribution_model 
#' @param list_arguments 
#' @param order 
#' @param offset 
#' @param concProb_type 
#' @param nu 
#' @param location_glm_results 
#'
#' @returns
#' @export
run_xgboost <- function(train, 
                        test, 
                        variables, 
                        target_variable, 
                        distribution_model, 
                        objective = "count:poisson",
                        booster = "gbtree", 
                        ..., 
                        nrounds = 100, 
                        params = list(), 
                        nthread = 4, 
                        order = 1,
                        offset = "exposure", 
                        concProb_type = "bin", 
                        nu = 100, 
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
  # no variable subset
  
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
  
  ### Model ####
  results_output["Model"] <- list(trained_model)
  
  return(results_output)
}