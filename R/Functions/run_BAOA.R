#' Running function for the BAOA
#'
#' @inheritParams run_GA 
#' @param beta Parameter beta for the BAOA algorithm
#' @param k Parameter beta for the BAOA algorithm
#' @param minMoa Parameter beta for the BAOA algorithm 
#' @param maxMoa Parameter beta for the BAOA algorithm
#' @param transferFun Transfunction object for the BAOA algorithm
#' @param ... Not used
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
run_BAOA <- function(train, 
                   test, 
                   variables, 
                   target_variable, 
                   distribution_model, 
                   beta, 
                   k, 
                   minMoa, 
                   maxMoa, 
                   pop_size, 
                   transferFun, 
                   max_stable, 
                   max_iter, 
                   ..., 
                   seed = 42,
                   order = 1,
                   offset = "exposure", 
                   concProb_type = "bin", 
                   withMain = TRUE, 
                   nu = 100, 
                   location_glm_results = NULL) {
  
  
  # Construction of the Variable Handler for the variables
  VH <- VariableHandler$new(variables = variables, 
                            order = order)
  
  # get the target values 
  y <- train[[target_variable]]
  
  if (!is.null(offset)) {
    offset_model_train <- train[[offset]]
    offset_model_test <- train[[offset]]
  } else {
    offset_model_train <- NULL
    offset_model_test <- NULL
  }
  
  
  # -------------------------------------------------------------------------
  # creation of the Particle Generator
  BAOA_gen <- BPG$new(ParticleBAOA,
                      chance_bit = 0.2,
                      suggestions = NULL)
  
  BAOA_swarm <- SwarmBAOA$new(pop_size, 
                              VH$get_length(), 
                              transferFun = transferFun, 
                              BAOA_gen, 
                              beta = beta, 
                              k = k, 
                              minMoa = minMoa, 
                              maxMoa = maxMoa, 
                              seed = seed)
  
  BAOA_run <- BAOA_swarm$run_process(retrieve_or_calculate_glm_coding, 
                                     max_stable = max_stable, 
                                     max_iter = max_iter,
                                     args_fun = list(
                                       train = train, 
                                       test = test, 
                                       VH = VH, 
                                       target_variable = target_variable, 
                                       distribution_model = distribution_model, 
                                       concProb_type = concProb_type,
                                       offset = offset, 
                                       withMain = withMain, 
                                       nu = nu, 
                                       location_data = location_glm_results
                                     ), 
                                     seed = 2 * seed)

  # -------------------------------------------------------------------------
  
  # get the results 
  results_output <- list("VariableImportance" = NULL, 
                         "VariableSubset" = NULL, 
                         "ConcProbTrainModel" = NULL, 
                         "ConcProbTestModel" = NULL, 
                         "ConcProbTrainGLM" = NULL, 
                         "ConcProbTestGLM" = NULL, 
                         "Model" = NULL, 
                         "AdditionalInfo" = NULL)
  
  ### Variable Importance ####
  results_output[["VariableImportance"]] <- variable_importance(results = BAOA_run$AllResults, 
                                                                column_name = "ConcProbTestGLM", 
                                                                col_binary = "Position", 
                                                                var_names = VH$get_used_variables())
  
  ### Variable Subset ####
  variables_selected <- VH$get_variables(BAOA_run$BestResult$Position, 
                                         withMain = withMain)
  results_output[["VariableSubset"]] <- variables_selected
  
  
  ### ConcProbTrainModel ####
  
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
                                           withMain = TRUE)
  
  # both the same models
  results_output["ConcProbTrainModel"] <- results_glm["ConcProbTrainGLM"]
  results_output["ConcProbTestModel"] <- results_glm["ConcProbTestGLM"]
  
  results_output["ConcProbTrainGLM"] <- results_glm["ConcProbTrainGLM"]
  results_output["ConcProbTestGLM"] <- results_glm["ConcProbTestGLM"]
  
  ### Model ####
  results_output["AdditionalInfo"] <- list(BAOA_run$AllResults)
  
  return(results_output)
}