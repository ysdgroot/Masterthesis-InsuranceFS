
#' Title
#'
#' @param testdata 
#' @param model 
#' @param col_observations 
#' @param ... 
#'
#' @return
#' 
#' @family evaluation functions
#' 
#' @export
evaluation_concProb_binned <- function(testdata, 
                                        model, 
                                        col_observations = "observed", 
                                        ...){
  
  #TODO: check if column exists

    #get the results from the model. 
  predModel <- predict(model, 
                       testdata, 
                       type = 'response')
  
  testdata[, predicted := predModel]
  
  result <- concProb_bin_fast(testdata[[col_observations]], testdata$predicted)$concProb
  
  return(list(result = result))
}

#' Title
#'
#' @inheritParams evaluation_concProb_binned
#' @param nu 
#' 
#' @return 
#' @family evaluation functions
#' 
#' @export
evaluation_concProb_continous <- function(testdata, 
                                       model, 
                                       col_observations = "observed",
                                       nu = 0,
                                       ...){
  
  #TODO: check if column exists
  
  #get the results from the model. 
  predModel <- predict(model, 
                       testdata, 
                       type = 'response')
  
  testdata[, predicted := predModel]
  
  result <- concProb_cont_fast(testdata[[col_observations]], 
                              testdata$predicted, 
                              nu = nu)$concProb
  
  return(list(result = result))
}



