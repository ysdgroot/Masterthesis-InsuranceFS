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
    
    # get the positions 
    temp_res_pos <- data.frame(Iteration = integer(), 
                               Position = character())
    for (pos in element_iter$Positions){
      temp_res_pos <- rbind(temp_res_pos, 
                            data.frame(Iteration = iter,
                                       Position = paste(pos, collapse = ''))) 
    }
    
    temp_res <- data.frame()
    for (res in element_iter$AllResults){
      temp_res <- rbind(temp_res, 
                        data.frame(res))
    }
    
    # combine positions
    temp_tot_res <- cbind(temp_res_pos, 
                          temp_res)
    
    list_results <- append(list_results, list(temp_tot_res))
  }
  return(rbindlist(list_results))
}
