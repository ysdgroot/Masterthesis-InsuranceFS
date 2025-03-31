#' Helper function to find the best lambda for the Elastic Net model. 
#' This will just use the [cv.glmnet()] function
#'
#' @param train data.table to use for the training
#' @param variables variables to be used for the model 
#' @param target_variable target variable
#' @param distribution_model distribution of the model 
#' @param alpha alpha parameter for the Elastic Net
#' @param ... extra parameters for the [cv.glmnet()]
#' @param order order of interactions
#' @param offset column name of `train` to define the offset
#' @param parallel if parallel training is necessary
#' @param nfolds number of folds for the search
#'
#' @returns list with "MinLambda" best lambda value 
#' "Model": the trained model
#' @export
best_elasticnet_lambda <- function(train, 
                                   variables, 
                                   target_variable, 
                                   distribution_model, 
                                   alpha = 1,  
                                   ..., 
                                   order = 1,
                                   offset = "exposure", 
                                   parallel = TRUE, 
                                   nfolds = 5){
  # Construction of the Variable Handler for the variables
  VH <- VariableHandler$new(variables = variables, 
                            order = order)
  
  # convert the columns into numeric values for the variable importance 
  train[, (variables) := lapply(.SD, as.numeric), 
        .SDcols = variables]
  
  # get the target values 
  y <- train[[target_variable]]
  if (!is.null(offset)) {
    offset_model_train <- train[[offset]]
  } else {
    offset_model_train <- NULL
  }
  
  # Construction of the sparse matrix 
  sparse_matrix_train <- sparse.model.matrix(VH$get_formula(coding = rep(1, VH$get_length()), 
                                                            distMod = distribution_model, 
                                                            targetVar = target_variable), 
                                             data = train)[,-1]
  
  model_cv <- cv.glmnet( sparse_matrix_train, 
                         y, 
                         family = distribution_model, 
                         offset = offset_model_train, 
                         parallel = parallel, 
                         nfolds = nfolds, 
                         alpha = alpha, 
                         ...)
  return(list("MinLambda" = model_cv$lambda.min, 
         "Model" = model_cv ))
}