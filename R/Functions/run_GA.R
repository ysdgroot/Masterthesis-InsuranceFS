#' Running function for the GA 
#'
#' @param train,test data.table with train/test data used to train the model or test the results 
#' @param variables vector with character. 
#' All the main variables to be used for the modelling. 
#' These should be found as column names in `train` and `test` 
#' @param target_variable character, 
#' column name of the response variable. 
#' Should be found in `train` and `test` 
#' @param distribution_model object of class family, to be used for the GLM modelling. 
#' @param order integer, 
#' Order of the interactions.
#' Recommend to keep this small, otherwise the model will take a long time to converge.  
#' @param offset character or NULL, to indicate the offset of the GLM model. 
#' @param nu positive integer. 
#' Value used for the Concordance probability when it is performed on continuous version
#' @param seed integer, for reproducibility
#' @param concProb_type "bin" or "cont", 
#' to indicate if the Concordance Probability should be performed on integers (bin) or continuous (cont) scale?
#' @param withMain logical, if the Main variables should be included if the interaction is used. 
#' This is only necessary when `order` is greater than 1
#' @param location_glm_results NULL or folder location to indicate where certain results should be stored or retrieved. 
#' 
#' @param selection The type of selection process for the GA algorithm (see [GA::ga()])
#' @param p_crossover Probability of crossover for the GA algorithm (see [GA::ga()])
#' @param p_mutation Probability of mutation for the GA algorithm (see [GA::ga()])
#' @param n_elits Number of elits to be kept for the next round (see [GA::ga()])
#' @param pop_size Population size of the Population based algorithm. 
#' @param max_stable Integer,
#' Maximum number of iterations with the same result. 
#' This is for early stopping the process. 
#' @param max_iter Integer, 
#' Maximum number of iterations
#' @param ... Not used 
#' @param parallel if the run should be done in parallel

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
  
  ### AdditionalInfo ####
  results_output["AdditionalInfo"] <- list(all_results)
  
  return(results_output)
}