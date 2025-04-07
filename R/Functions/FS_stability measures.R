#' Jaccard index for feature selection
#'
#' @param s1,s2 binary vectors of the same length
#'
#' @returns value between 0 and 1
#' @export
jaccard_index <- function(s1, s2) {
  sum(s1*s2)/sum((s1+s2) > 0)
}

#' Pearson Correlation for feature selection
#'
#' @inheritParams jaccard_index
#'
#' @returns correlation of 2 variables
#' @export
pearson_cor <- function(s1, s2) {
  if(all(s1 == s2)) {
    return(1)
  }
  cor(s1, s2, method = "pearson")
}

#' Average Pairwise Similarity measure
#'
#' @param feature_sets list with binary vectors
#' @param measure function which takes 2 binary vectors as input
#'
#' @returns numeric
#' @export
similarity_measure <- function(feature_sets, 
                               measure = pearson_cor) {
  
  # number of sets
  M <- length(feature_sets)
  
  total_measures <- 0
  for (i in 1:M) {
    for (j in 1:M) {
      if(i == j) {next}
      
      total_measures <- total_measures  +
        measure(feature_sets[[i]], feature_sets[[j]])
    }
  }
  result <- total_measures /(M * (M-1))
  return(result)
}
