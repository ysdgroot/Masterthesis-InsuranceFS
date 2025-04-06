# Helper functions ---------------------------------------------------------
# helper functions for the selection of the best values script

get_save_location <- function(algo, 
                              order, 
                              data_set_number){
  
  save_location <- file.path("Data", 
                             "Parameters", 
                             sprintf("Best_%s_Data%s_order_%s.RDS",
                                     algo,
                                     data_set_number,
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