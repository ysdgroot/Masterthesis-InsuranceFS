#' Compare the results give the best value
#' When the second value is better, it will give a logical TRUE for extra information
#'
#' @param value_1 the reference value
#' @param value_2 the 'new' value to compare with
#' @param is_minimize logical, if it should minimize or maximize
#' @param position if value_1 and value_2 are lists, then the position of the list
#'
#' @returns list wit 2 elements 
#' "BestValue" the best value of both
#' hasImproved logical if the value_2 is better compared to value_1 (depends if it should be minimized or not)
#' @export
#'
#' @examples
has_improved <- function(value_1, 
                         value_2, 
                         is_minimize = TRUE, 
                         position = 1){
  
  best_value <- value_1
  is_better <- FALSE
  if ((is_minimize & value_1[[position]] > value_2[[position]]) |
      (!is_minimize & value_1[[position]] < value_2[[position]])) {
    best_value <- value_2
    is_better <- TRUE
  }
  return(list(BestValue = best_value, 
              hasImproved = is_better))
}

#' Helper function to remove or add variables, depending the stepwise (forward/backward)
#'
#' @param total_list The already selected list of variables
#' @param element variable of question (should this be added or removed)
#' @param is_remove logical, if variable should be removed or added
#'
#' @returns all variables necessary for the next step
#' @export
add_remove <- function(total_list, 
                       element, 
                       is_remove = FALSE){
  if (is_remove) {
    list_to_send <- setdiff(total_list, 
                            element)
  } else {
    list_to_send <- c(total_list, 
                      element)
  }
  return(list_to_send)
}