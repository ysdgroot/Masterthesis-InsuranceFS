# Importing library -------------------------------------------------------

source(file.path("R", "Packages.R"))

source(file.path("R", "General Parameters.R"))
source(file.path("R", "Config-order1.R"))

# Parameter Selection  ----------------------------------------------------
# Best selection by Best found and Least number of iterations 
beta <- seq(4.9, 5.1, 0.05)
k <- seq(0.2, 0.8, 0.1)
minMoa <- seq(0.1, 0.4, 0.1)
maxMoa <- seq(0.6, 0.9, 0.1)
pop_size <- seq(10, 25, 5)

base_tests <- expand.grid("beta" = beta, 
                          "k" = k, 
                          "minMoa" = minMoa, 
                          "maxMoa" = maxMoa, 
                          "TransFun" = baseClassTransferFunctions, 
                          "Popsize" = pop_size)
setDT(base_tests)
base_tests[, ID := .I]

# save all the results
list_results <- list()

# base parameters 
max_iter <- 30 
max_stable <- 10

total_runs <- nrow(base_tests)

for (ifold in 1:nfolds) {
  cat(crayon::blue(sprintf("Start Fold: %d ------------------------------------------------  \n", 
                           ifold)))
  
  trainDT_fold <- inputDT[Fold != ifold]
  testDT_fold <- inputDT[Fold == ifold]
  
  # create folder to save results 
  folder_name <- sprintf(base_name_folder, 
                         seed,
                         ifold, 
                         nfolds, 
                         "bin", 
                         VH$get_length(), 
                         null_value)
  full_folder_name <- file.path("Data", folder_name)
  dir.create(full_folder_name, showWarnings = FALSE)
  
  for (i in 1:nrow(base_tests)) {
    cat(crayon::blue(sprintf("Fold: %d \t Run: %d/%d \n", 
                             ifold, 
                             i, 
                             total_runs)))
    
    BAOA_gen <- BPG$new(ParticleBAOA,
                       chance_bit = 0.2,
                       suggestions = NULL)
    BAOA_swarm <- SwarmBAOA$new(base_tests[i,]$Popsize, 
                                VH$get_length(), 
                                transferFun = base_tests[i,][["TransFun"]][[1]], 
                                BAOA_gen, 
                                beta = base_tests[i,]$beta, 
                                k = base_tests[i,]$k, 
                                minMoa = base_tests[i,]$minMoa, 
                                maxMoa = base_tests[i,]$maxMoa, 
                                seed = 420)
    
    BAOA_run <- BAOA_swarm$run_process(concProb_glm_fastglm, 
                                       max_stable = max_stable, 
                                       max_iter = max_iter,
                                       args_fun = list(
                                         trainDT = trainDT_fold, 
                                         testDT = testDT_fold, 
                                         distMod = poisson(link = "log"), 
                                         variableHandler = VH, 
                                         nullValue = null_value, 
                                         type = "bin", 
                                         offset = "exposure", 
                                         targetVar = "claimNumber", 
                                         location_save = full_folder_name
                                       ), 
                                       seed = 123)
    
    # save the results 
    
    result <- data.table("nIterations" = BAOA_swarm$get_iteration(), 
                   "BestResult" = BAOA_run$BestResult$Result, 
                   "ID" = i, 
                   "Fold" = ifold)
    cat(green(sprintf("ID: %d - nIterations: %d - Result: %g - Position: %s \n", 
                      i, 
                      BAOA_swarm$get_iteration(), 
                      BAOA_run$BestResult$Result, 
                      paste(BAOA_run$BestResult$Position, collapse = ""))))
    
    list_results <- append(list_results, list(result))
  }
}

dt_results_cv_baoa <- rbindlist(list_results)

dir.create(file.path("Data", 
                     "Parameters"), 
           showWarnings = FALSE)

#Use the name of the Transfer function, rather than the object
base_tests_temp <- copy(base_tests)
base_tests_temp[, TransFunName := as.character(lapply(TransFun, \(x) x$get_name()))]
base_tests_temp[, TransFun := NULL]

dt_results_cv_baoa <- dt_results_cv_baoa |> 
  collapse::join(base_tests_temp, 
       on = 'ID', 
       how = "left")

saveRDS(dt_results_cv_baoa, 
        sprintf(file.path("Data", 
                  "Parameters", 
                  "Param_BAOA_bin_order%d.RDS"), 
                order))

