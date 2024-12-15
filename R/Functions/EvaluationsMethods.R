
#' Title
#'
#' @param testDT 
#' @param model 
#' @param col_observations 
#' @param ... 
#'
#' @return
#' 
#' @family evaluation functions
#' 
#' @export
evaluation_concProb_binned <- function(testDT, 
                                        model, 
                                        col_observations = "observed", 
                                        ...){
  
  #TODO: check if column exists

    #get the results from the model. 
  predModel <- predict(model, 
                       testDT, 
                       type = 'response')
  
  testDT[, predicted := predModel]
  
  result <- concProb_bin_fast(testDT[[col_observations]], testDT$predicted)$concProb
  
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
evaluation_concProb_continous <- function(testDT, 
                                       model, 
                                       col_observations = "observed",
                                       nu = 0,
                                       ...){
  
  #TODO: check if column exists
  
  #get the results from the model. 
  predModel <- predict(model, 
                       testDT, 
                       type = 'response')
  
  testDT[, predicted := predModel]
  
  result <- concProb_cont_fast(testDT[[col_observations]], 
                              testDT$predicted, 
                              nu = nu)$concProb
  
  return(list(result = result))
}



