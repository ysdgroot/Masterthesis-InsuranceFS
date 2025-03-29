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
#' @param type 
#' @param nu 
#'
#' @returns
#' @export
run_GA <- function(train, 
                   test, 
                   variables, 
                   target_variable, 
                   distribution_model, 
                   selection, 
                   p_crossover, 
                   p_mutation, 
                   n_elits, 
                   pop_size, 
                   max_stable, 
                   max_iter, 
                   ..., 
                   parallel = TRUE, 
                   seed = 42,
                   order = 1,
                   offset = "exposure", 
                   concProb_type = "bin", 
                   nu = 100, 
                   withMain = TRUE, 
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

  
  GA_sim <- ga(fitness = retrieve_or_calculate_glm_coding, 
                 train = train, 
                 test = test, 
                 VH = VH, 
                 target_variable = target_variable, 
                 distribution_model = distribution_model, 
                 offset = offset, 
                 withMain = withMain,
                 location_data = location_glm_results,
                 nu = nu, 
               concProb_type = concProb_type, 
               type = "binary", # optimization data type
               population = gabin_Population,
               selection = selection, 
               crossover = gabin_uCrossover,  # cross-over method
               pcrossover = p_crossover,
               mutation = gabin_raMutation, # uniform random 
               pmutation = p_mutation, 
               elitism = n_elits, # best N indiv. to pass to next iteration
               popSize = pop_size, # number of the population in each evolution
               nBits = VH$get_length(), # total number of variables
               names = VH$get_used_variables(), # variable names
               run = max_stable, # max iter without improvement (stopping criteria)
               maxiter = max_iter, # total runs or generations
               keepBest = TRUE, # keep the best solution at the end
               parallel = parallel, # don't do parallel because it takes to much time 
               seed = seed)
  
  
  # put all the results into a data.table
  all_results <- convert_resultGA(GA_sim@allResults)
  
  # best results and position 
  best_result <- list(Position = GA_sim@solution, 
                      Result = GA_sim@fitnessValue)
  
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
  results_output[["VariableImportance"]] <- variable_importance(all_results, 
                                                                column_name = "ConcProbTestGLM", 
                                                                col_binary = "Position",
                                                                var_names = VH$get_used_variables())
  
  ### Variable Subset ####
  variables_selected <- names(best_result$Position[1,])[best_result$Position[1,] == 1]
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
                                           type = type, 
                                           nu = nu, 
                                           location_data = location_glm_results, 
                                           withMain = TRUE)
  
  # both the same models
  results_output["ConcProbTrainModel"] <- results_glm["ConcProbTrainGLM"]
  results_output["ConcProbTestModel"] <- results_glm["ConcProbTestGLM"]
  
  results_output["ConcProbTrainGLM"] <- results_glm["ConcProbTrainGLM"]
  results_output["ConcProbTestGLM"] <- results_glm["ConcProbTestGLM"]
  
  ### AdditionalInfo ####
  results_output["AdditionalInfo"] <- all_results
  
  return(results_output)
}