
#' Get the full inheritance of an object
#'
#' @param R6ClassGenerator R6ClassGenerator object
#'
#' @returns character with all the names even the start name of the R6ClassGenerator
#' @export
get_full_inheritance <- function(R6ClassGenerator){
  
  if(is.null(R6ClassGenerator)){
    return(NULL)
  }
  
  if(class(R6ClassGenerator) != "R6ClassGenerator"){
    stop("Object needs to be an R6ClassGenerator")
  }
  parent_obj <- R6ClassGenerator$get_inherit()
  
  return(c(R6ClassGenerator$classname,get_full_inheritance(parent_obj)))
}
