#' Running function for the Stepwise
#'
#' @inheritParams run_GA 
#' @param type_run "forward" or "backward" for the type of stepwise it should perform
#' @param ... Not used
#' @param is_minimize logical, 
#' if the function is a minimization problem of maximazation
#' @param position_output if output is a list, which position of the list should be used. 
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
run_stepwise <- function(train, 
                        test, 
                        variables, 
                        target_variable, 
                        distribution_model, 
                        type_run = "forward", 
                        ..., 
                        is_minimize = FALSE, 
                        position_output = 1, 
                        order = 1,
                        offset = "exposure", 
                        concProb_type = "bin", 
                        nu = 100, 
                        withMain = TRUE, 
                        location_glm_results = NULL) {
  
  # Construction of the Variable Handler for the variables
  VH <- VariableHandler$new(variables = variables, 
                            order = order)
  
  if (!is.null(offset)) {
    offset_model_train <- train[[offset]]
    offset_model_test <- test[[offset]]
  } else {
    offset_model_train <- NULL
    offset_model_test <- NULL
  }
  
  # setup
  if (type_run == "forward") {
    var_to_use <- c()
    is_remove <- FALSE
    
  } else if (type_run == "backward") {
    var_to_use <- VH$get_all_variables()
    is_remove <- TRUE
  }
  
  # construct the base model 
  base_model <- retrieve_or_calculate_glm(train = train, 
                                          test = test, 
                                          VH = VH, 
                                          variables = var_to_use, 
                                          target_variable = target_variable, 
                                          distribution_model = distribution_model, 
                                          offset = offset, 
                                          concProb_type = concProb_type, 
                                          nu = nu, 
                                          location_data = location_glm_results, 
                                          withMain = withMain)
  
  # setup variables
  is_run <- TRUE
  best_var_change <- c()
  best_result <- base_model
  run_number <- 0
  while (is_run) {
    run_number <- run_number + 1
    # get all the variables which needs to be checked
    if (is_remove){
      variables_to_test <- var_to_use
      
    } else {
      variables_to_test <- unique(add_remove(VH$get_all_variables(), 
                                             var_to_use, 
                                             is_remove = TRUE)) 
    }
    
    is_run <- FALSE

    for (ivar in seq_along(variables_to_test)) {
      var <- variables_to_test[[ivar]]
      
      if (is_remove) {
        var_to_use <- setdiff(var_to_use, 
                              var)
      } else {
        var_to_use <- c(var_to_use, var)
      }
      
      
      cat(sprintf("\r Run [%d] - [%d/%d] Variable: %s \033[K", 
                  run_number, 
                  ivar, 
                  length(variables_to_test),
                  var))
      
      result <- retrieve_or_calculate_glm(train = train, 
                                          test = test, 
                                          VH = VH, 
                                          variables = var_to_use, 
                                          target_variable = target_variable, 
                                          distribution_model = distribution_model, 
                                          offset = offset, 
                                          concProb_type = concProb_type, 
                                          nu = nu, 
                                          location_data = location_glm_results, 
                                          withMain = withMain)
      
      # test if the result are better
      is_better_result <- has_improved(best_result, 
                                       result, 
                                       is_minimize = is_minimize, 
                                       position = position_output)$hasImproved
      
      if (is_better_result){
        best_result <- result
        best_var_change <- var
        is_run <- TRUE
      }
      
      if (is_remove) {
        var_to_use <- c(var_to_use, var)
      } else {
        var_to_use <- setdiff(var_to_use, 
                              var)
      }
    }
    
    if (is_run){
      text_add_removed <- ifelse(is_remove, "removed", "added")
      cat(sprintf("\t Variable [%s] is %s; Value %s \n", 
                  best_var_change, 
                  text_add_removed,
                  best_result[[position_output]]))
      # taking opposite of "is_remove"
      # forward it should add 
      # backward it should remove 
      var_to_use <- add_remove(var_to_use, 
                               best_var_change, 
                               is_remove = is_remove)
      best_var_change <- c()
    }
  }
  cat("\n")
  
  # get the results 
  results_output <- list("VariableImportance" = NULL, 
                         "VariableSubset" = NULL, 
                         "ConcProbTrainModel" = NULL, 
                         "ConcProbTestModel" = NULL, 
                         "ConcProbTrainGLM" = NULL, 
                         "ConcProbTestGLM" = NULL, 
                         "Model" = NULL)
  
  ### Variable Importance ####
  # no variable importance
  
  ### Variable Subset ####
  results_output["VariableSubset"] <- list(var_to_use)
  ### ConcProbTrainModel ####

  # GLM Variable Modelling --------------------------------------------------
  
  # get the results when running the 'standard' GLM 
  results_glm <- retrieve_or_calculate_glm(train = train, 
                                           test = test, 
                                           VH = VH, 
                                           variables = var_to_use, 
                                           target_variable = target_variable, 
                                           distribution_model = distribution_model, 
                                           offset = offset, 
                                           concProb_type = concProb_type, 
                                           nu = nu, 
                                           location_data = location_glm_results, 
                                           withMain = withMain)
  
  # both the same models
  results_output["ConcProbTrainModel"] <- results_glm["ConcProbTrainGLM"]
  results_output["ConcProbTestModel"] <- results_glm["ConcProbTestGLM"]
  
  results_output["ConcProbTrainGLM"] <- results_glm["ConcProbTrainGLM"]
  results_output["ConcProbTestGLM"] <- results_glm["ConcProbTestGLM"]
  
  ### Model ####
  results_output["Model"] <- NULL
  
  return(results_output)
}