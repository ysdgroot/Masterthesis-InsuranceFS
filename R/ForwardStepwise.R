# Load packages -----------------------------------------------------------
# 
# source(file.path("R", "Packages.R"))
# source(file.path("R", "General Parameters.R"))
# source(file.path("R", "Config-order2.R"))

# Forward -----------------------------------------------------------------




# Running function -------------------------------------------------------


# Forward = minimize --> Backward = maximize
# Forward = maximize --> Backward = minimize 

# Forward = adding --> Backward = remove
# Forward = empty list --> Backward = full list 

#TODO: create function out of it
position_result <- 4
is_minimize <- TRUE
type_selection <- "forward" # "backward"


for (i_fold in 1:nfolds){
  
  if (type_selection == "forward") {
    var_to_use <- c()
    is_remove <- FALSE
    is_minimize <- is_minimize
    
  } else if (type_selection == "backward") {
    var_to_use <- VH$get_all_variables()
    is_remove <- TRUE
    # when backward the variable will be selected based on the highest decrease
    is_minimize <- !is_minimize 
  }
  
  # create folder to save results 
  folder_name <- sprintf(base_name_folder, 
                         seed,
                         i_fold, 
                         nfolds, 
                         "bin", 
                         VH$get_length(), 
                         null_value)
  full_folder_name <- file.path("Data", folder_name)
  
  if (!dir.exists(full_folder_name)) {
    dir.create(full_folder_name, 
               recursive = TRUE)
  }
  trainDT <- inputDT[Fold != i_fold]
  testDT <- inputDT[Fold == i_fold]
  
  base_model <- concProb_glm_fastglm(VH$get_coding(var_to_use), 
                                      trainDT = trainDT, 
                                      testDT = testDT, 
                                      variableHandler = VH, 
                                      offset = "exposure", 
                                      targetVar = "claimNumber", 
                                      withMain = TRUE,
                                      distMod = poisson(link = "log"), 
                                      nullValue = 0, 
                                      location_save = full_folder_name)
  
  is_run <- TRUE
  
  #TODO: change the name, because now it is forward and backward add/remove
  best_var_to_add <- c()
  best_result <- base_model
  run_number <- 0
  while (is_run) {
    run_number <- run_number + 1
    variables_to_test <- add_remove(VH$get_all_variables(), 
                                    var_to_use, 
                                    is_remove = is_remove)  
    is_run <- FALSE
    browser()
    for (ivar in seq_along(variables_to_test)) {
      var <- variables_to_test[[ivar]]
      
      cat(sprintf("\r Fold [%d/%d] - Run [%d] - [%d/%d] Variable: %s \033[K", 
                  i_fold, 
                  nfolds, 
                  run_number, 
                  ivar, 
                  length(variables_to_test),
                  var))
      
      result <- concProb_glm_fastglm(VH$get_coding(c(var_to_use, var)), 
                                     trainDT = trainDT, 
                                     testDT = testDT, 
                                     variableHandler = VH, 
                                     offset = "exposure", 
                                     targetVar = "claimNumber", 
                                     withMain = TRUE,
                                     distMod = poisson(link = "log"), 
                                     nullValue = 0, 
                                     location_save = full_folder_name)
    
      #TODO: rename "has_improved" to something else for "forward" and "backward" selection
      is_better_result <- has_improved(best_result, 
                                     result, 
                                     is_minimize = is_minimize, 
                                     position = position_result)$hasImproved
      
      if (is_better_result){
        best_result <- result
        best_var_to_add <-var
        is_run <- TRUE
      }
    }
    
    if (is_run){
      cat(sprintf("\t Variable [%s] is added; Value %s \n", 
                  best_var_to_add, 
                  best_result[[position_result]]))
      # taking opposite of "is_remove"
      # forward it should add 
      # backward it should remove 
      var_to_use <- add_remove(var_to_use, 
                               best_var_to_add, 
                               is_remove = !is_remove)
      best_var_to_add <- c()
    }
  }
}


# Backwards ---------------------------------------------------------------
# remove if it lowers/higher when maximizing/minimizing 

# remove only the one with the highest difference 
# parts of the code from the forward stepwise can be used 







