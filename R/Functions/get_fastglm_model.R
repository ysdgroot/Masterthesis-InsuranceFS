#' Helper function to train a glm model 
#'
#' @inheritParams construct_modelmatrix
#' @param method method for the fastglm model. 
#' See function [fastglm()] for more information.
#'
#' @returns fastglm model using the variables 
#' @export
get_fastglm_model <- function(data, 
                               variables, 
                               target_variable, 
                               distMod, 
                               offset = NULL, 
                               method = 3){
  
  mm <- construct_modelmatrix(data = data, 
                              variables = variables, 
                              target_variable = target_variable, 
                              distMod = distMod, 
                              offset = offset)
  
  # make the model 
  fitModel <- fastglm(x = mm, 
                      y = data[[target_variable]], 
                      family = distMod, 
                      data = data, 
                      method = method)
  
  return(fitModel)
}


#' Helper function to transform a data set into a model matrix. 
#'
#' @param data data.table to be used
#' @param variables the variables to be used. 
#' Interactions are also possible. These variables will be used to transform into a formula. 
#' @param target_variable the target Variable
#' @param distMod Distribution model for the GLM. This is used for the offset 
#' @param offset  column of the offset, in case there is an offset. 
#' If none, NULL can be used
#'
#' @returns model.matrix
#' @export
construct_modelmatrix <- function(data, 
                                  variables, 
                                  target_variable, 
                                  distMod, 
                                  offset = NULL){
  # construction of the formula
  formula_glm <- construct_formula(variables = variables, 
                                   target_variable = target_variable, 
                                   distMod = distMod, 
                                   offset = offset)
  # construction of the model matrix
  mm <- model.matrix(formula_glm, 
                     data = data)
  
  return(mm)
}

