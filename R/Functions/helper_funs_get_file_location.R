# Helper functions ---------------------------------------------------------
# helper functions for the selection of the best values script

get_save_location <- function(algo, 
                              order){
  
  save_location <- file.path("Data", 
                             "Parameters", 
                             sprintf("Best_%s_order_%d.RDS",
                                     algo,
                                     order))
  return(save_location)
}

get_file_location <- function(algo, 
                              order, 
                              type = "bin"){
  file_location <- file.path("Data", 
                             "Parameters", 
                             sprintf("Param_%s_%s_order%d.RDS",
                                     algo, 
                                     type,
                                     order))
  
  return(file_location)
}