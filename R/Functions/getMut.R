#' Get the mutations
#'
#' @param mat matrix  with 0 or 1, where 0 is not selecting the variable and 1 selecting
#' @param nMut number of mutations per generations
#'
#' @return The newly created matrix with the selected variables
#' @export
getMut <- function(mat, 
                   nMut){
  nStartMods <- nrow(mat)
  nVar <- ncol(mat)
  for(iRow in 1:nStartMods){
    rowSampPos <- sort(sample(1:nVar, nMut))
    mat[iRow, rowSampPos] = abs(mat[iRow, rowSampPos] - 1)
  }
  return(mat)
}