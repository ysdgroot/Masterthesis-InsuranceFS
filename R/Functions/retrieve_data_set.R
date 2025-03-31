#' Helper function to retrieve the necessary datasets
#'
#' @param data_set_number number of the data set
#' @param location location where the data sets can be found
#'
#' @returns results of the data set. 
#' This should be a list with values 
#' Data 
#' Variables
#' Target (variable)
#' Offset
#' ConcProbType
#' Distribution
#' @export
retrieve_data_set <- function(data_set_number, 
                              location = file.path("Data","Datasets")) {
  
  return(readRDS(file = file.path(location, 
                                  sprintf("DataSet_%s.RDS", 
                                          data_set_number))))
  
}
