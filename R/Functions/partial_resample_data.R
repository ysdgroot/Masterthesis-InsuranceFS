#' Helper function to split data into train and test set with replacement
#' A percentage is given to split the data into that size. 
#'
#' @param dt data.table with the data which needs to be sampled
#' @param perc_value value between 0 and 1. 
#' It will not give any errors when the value is above 1. 
#' This is used to calculate the size of the training set. 
#' @param seed integer, for reproducibility
#'
#' @returns list with "Train" and "Test"
#' Train will be all the values selected with resampling.
#' Test will be all the values not in the training set. 
#' If the `perc_value` is too big, it is possible no test set will be there.  
#' @export
partial_resample_data <- function(dt, 
                                  perc_value, 
                                  seed = 123) {
  
  set.seed(seed)
  nrows_data <- nrow(dt)
  total_sample <- round(perc_value * nrows_data)
  
  samples <- sample(nrows_data, 
                    size = total_sample, 
                    replace = TRUE)

  return(list("Train" = dt[samples], 
              "Test" = dt[!samples]))
}


