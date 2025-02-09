#' Get the results sorted based on the average results of the folds, 
#' As second parameter, the number of iterations
#'
#' @param all_results data.table with columns 'nIterations' and 'BestResult'
#' and 'ID'
#'
#' @returns ordered and grouped (combined the folds) data.table. 
#' the order are the best results and then the least number of iterations
#' @export
best_param_run <- function(all_results){
  base_tests <- unique(all_results[, .SD, 
                                   .SDcols = !c("nIterations", 
                                                "BestResult")])
  
  avg_results <- all_results[, .(AvgResult = mean(BestResult),
                                 AvgIteration = mean(nIterations)),
                             by = c("ID")][order(-AvgResult, AvgIteration)] 
  
  
  avg_results <- avg_results |> 
    collapse::join(base_tests, 
                   on = "ID", 
                   how = "left")
  
  # remove the Fold column 
  avg_results[, Fold := NULL]
  
  return(avg_results[order(-AvgResult)])
}
