#' Helper function to construct a formula based on the variables
#'
#' @param variables variables to be used for a formula. 
#' Hence interactions can also be used. 
#' @param target_variable target variable for the model
#' @param distMod Distribution model for the GLM. 
#' This is only used for the offset
#' @param offset column of the offset, in case there is an offset
#'
#' @returns a formula
#' @export
construct_formula <- function(variables, 
                              target_variable,
                              distMod, 
                              offset = NULL){
  linkFunc <- distMod$link
  form <- paste0(target_variable, "~ ")
  
  if (!is.null(offset)){
    form <- paste0(form, sprintf("offset(%s(%s)) + ",linkFunc, offset))
  }
  
  if (length(unlist(variables)) > 0 ){
    form <- paste0(form, paste0(unlist(variables), collapse = "+"))
  } else {
    form <- paste0(form, "1")
  }
  
  form <- as.formula(form)
  return(form)
}