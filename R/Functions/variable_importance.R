#' Title
#'
#' @param results 
#' @param column_name 
#' @param col_binary 
#'
#' @returns
#' @export
#'
#' @examples
variable_importance <- function(results, 
                                column_name, 
                                col_binary = "Position", 
                                var_names = NULL){
  
  c_results <- copy(results)
  
  c_results[, ConvertedPosition := lapply(strsplit(get(col_binary) , 
                                                   split = ""), 
                                          as.numeric)]
  c_results[, ValuePosition := map2(ConvertedPosition, 
                                    get(column_name), 
                                    \(x, y) x * y)]
  
  n_features <- length(c_results$ValuePosition[[1]])
  
  # construction of a matrix
  # unlist is necessary due to the construction of the results
  mat_test <- matrix(unlist(c_results$ValuePosition), 
                     ncol = n_features, 
                     byrow = TRUE)
  
  # get the average result when used 
  avg_when_used <- colSums(mat_test) / colSums(mat_test != 0)
  
  # get the average result when variable is not used 
  avg_not_used <- c()
  
  for (iter in 1:n_features) {
    mat_test[which(mat_test[,iter] == 0),]
    
    # average when the column is not used 
    avg_not_used_iter <- mean(rowMaxs(mat_test[which(mat_test[,iter] == 0),]))
    avg_not_used <- c(avg_not_used,
                      avg_not_used_iter)
  }
  
  result <- avg_when_used - avg_not_used
  
  if (!is.null(var_names)) {
    names(result) <- var_names
  }
  
  return(result)
}

