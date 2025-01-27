#' Transform the list of results into a data.table format
#'
#' @param results list of results based on the [run_process()] of the class
#' `BinarySwarm`. Only the element of `AllResults` is necessary to give. 
#'
#' @returns data.table with all the results
#' @export
result_2_dt <- function(results){
  
  list_results <- list()
  for (iter in seq_along(results)){
    element_iter <- results[[iter]]
    
    temp_res <- data.frame()
    for (res in element_iter$AllResults){
      temp_res <- rbind(temp_res, 
                        data.frame(Iteration = iter, 
                                   res))
    }
    
    # combine positions
    list_results <- append(list_results, list(temp_res))
  }
  return(rbindlist(list_results))
}
