# Importing library -------------------------------------------------------

source(file.path("R", "0-Packages.R"))

source(file.path("R", "1-General Parameters.R"))
source(file.path("R", "1-Config-order2.R"))

# Parameter selection is based on the best results of order 1
# otherwise to much compute time 

stop("Not Implemented Yet")

# Parameter Selection  ----------------------------------------------------
# Best selection by Best found and Least number of iterations 
k1 <- seq(1, 6, 0.5) 
k2 <- seq(1, 6, 0.5)
w <- seq(0.1, 2.1, 0.5)
pop_size <- seq(10, 25, 5)

base_tests <- expand.grid("k1" = k1, 
                          "k2" = k2, 
                          "w" = w, 
                          "TransFun" = baseClassTransferFunctions, 
                          "Popsize" = pop_size)
setDT(base_tests)
base_tests[, ID := .I]

# save all the results
list_results <- list()

# base parameters 
max_iter <- 30 
max_stable <- 10
withMain <- TRUE

total_runs <- nrow(base_tests)

for (ifold in 1:nfolds) {
  cat(crayon::blue(sprintf("Start Fold: %d ---------------- \n", 
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
    
    BPSO_gen <- BPG_Velocity$new(ParticleBPSO, 
                                 chance_bit = 0.2,
                                 suggestions = NULL)
    BPSO_swarm <- SwarmBPSO$new(base_tests[i,]$Popsize, 
                                VH$get_length(), 
                                transferFun = base_tests[i,][["TransFun"]][[1]], 
                                BPSO_gen, 
                                w = base_tests[i,]$w, 
                                k1 = base_tests[i,]$k1, 
                                k2 = base_tests[i,]$k2, 
                                seed = 420)
    
    BPSO_run <- BPSO_swarm$run_process(concProb_glm_fastglm, 
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
                                         location_save = full_folder_name, 
                                         withMain = withMain
                                       ), 
                                       seed = 123)
    
    # save the results 
    
    result <- data.table("nIterations" = BPSO_swarm$get_iteration(), 
                   "BestResult" = BPSO_run$BestResult$Result, 
                   "ID" = i, 
                   "Fold" = ifold)
    cat(green(sprintf("ID: %d - nIterations: %d - Result: %g - Position: %s \n", 
                      i, 
                      BPSO_swarm$get_iteration(), 
                      BPSO_run$BestResult$Result, 
                      paste(BPSO_run$BestResult$Position, collapse = ""))))
    
    list_results <- append(list_results, list(result))
  }
}

dt_results_cv_bpso <- rbindlist(list_results)

dir.create(file.path("Data", 
                     "Parameters"), 
           showWarnings = FALSE)

#Use the name of the Transfer function, rather than the object
base_tests_temp <- copy(base_tests)
base_tests_temp[, TransFunName := as.character(lapply(TransFun, \(x) x$get_name()))]
base_tests_temp[, TransFun := NULL]

dt_results_cv_bpso <- dt_results_cv_bpso |> 
  collapse::join(base_tests_temp, 
                 on = 'ID', 
                 how = "left")

saveRDS(dt_results_cv_bpso, 
        sprintf(file.path("Data", 
                          "Parameters", 
                          "Param_BPSO_bin_order%d.RDS"), 
                order))

