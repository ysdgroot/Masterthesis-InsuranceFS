#' Helper function to retrieve or calculate the GLM model. 
#' To improve the speed. 
#'
#' @inheritParams results_variables_glm
#' @param VH a VariableHandler object. 
#' This is used to transform the variables into a coding (depending on the variables and the order of interactions)
#' @param location_data folder to look at to retrieve previous results. 
#' The folder should then contain .RDS files with the binary coding as name. 
#' @param withMain logical, 
#' if the main variable should be included in case interactions are used. 
#'
#' @returns result of the GLM model. 
#' list with two values, ConcProbTestGLM and ConcProbTrainGLM. 
#' Each gives the Concordance Probability for the test and train dataset 
#' @export
retrieve_or_calculate_glm <- function(train, 
                                      test, 
                                      VH, 
                                      variables, 
                                      target_variable, 
                                      distribution_model, 
                                      location_data = NULL, 
                                      offset = NULL,
                                      withMain = TRUE, 
                                      concProb_type = "bin", 
                                      nu = 100) {
  
  if (!(concProb_type %in% c("bin","cont"))) {
    stop(sprintf("The value for 'concProb_type' should be 'bin' or 'cont' not %s", 
                 concProb_type))
  }
  
  # name of the file is the coding of the variables
  # need to do this, otherwise results without main variables are not correct
  coding <- VH$get_coding(variables,  
                          withMain = withMain)
  # in case not all main variables are included
  all_variables <- VH$get_variables(coding) 
  
  # first to check
  if (!is.null(location_data)){
    if(!dir.exists(location_data)){
      stop(sprintf("The folder %s does not exist"))
    }
    
    file_location <- here::here(location_data, 
                               sprintf("%s.RDS", 
                                       paste(coding, 
                                             collapse = "")))
    
    # if file exists then collects the information 
    if (file.exists(file_location)) {
      results <- readRDS(file_location)
      return(results)
    }
  }
  
  # calculate the results 
   results <- results_variables_glm(train = train, 
                                    test = test, 
                                    variables = all_variables, 
                                    target_variable = target_variable, 
                                    distribution_model = distribution_model, 
                                    offset = offset, 
                                    concProb_type = concProb_type, 
                                    nu = nu)
   
   # if not NULL it will save the results
   if (!is.null(location_data)) {
     saveRDS(results, 
             file = file_location)
   }
  
   return(results)
}


#' Helper function to retrieve or calculate the GLM model. 
#' To improve the speed. 
#' This is using the coding instead of the variables
#'
#' @param coding binary vector, usable for the variableHandler
#' @inheritParams retrieve_or_calculate_glm
#'
#' @returns result of the GLM model. 
#' list with two values, ConcProbTestGLM and ConcProbTrainGLM. 
#' Each gives the Concordance Probability for the test and train dataset 
#' @export
retrieve_or_calculate_glm_coding <- function(coding, 
                                             train, 
                                             test, 
                                             VH, 
                                             target_variable, 
                                             distribution_model, 
                                             location_data = NULL, 
                                             offset = NULL,
                                             withMain = TRUE, 
                                             concProb_type = "bin", 
                                             nu = 100) {
  
  # need to do this, otherwise results without main variables are not correct
  coding <- VH$get_coding(VH$get_variables(coding, 
                                            withMain = withMain))
  
  # in case not all main variables are included
  all_variables <- VH$get_variables(coding) 
  
  results <- retrieve_or_calculate_glm(train = train, 
                                        test = test, 
                                        VH = VH, 
                                        variables = all_variables, 
                                        target_variable = target_variable, 
                                        distribution_model = distribution_model, 
                                        location_data = location_data, 
                                        offset = offset,
                                        withMain = withMain, 
                                       concProb_type = concProb_type, 
                                        nu = nu)

  results[["Position"]] <- paste(coding, 
                                 collapse = "")
  return(results)
}
