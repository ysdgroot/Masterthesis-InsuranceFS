# Importing library -------------------------------------------------------

source(file.path("R", "Packages.R"))

source(file.path("R", "General Parameters.R"))
source(file.path("R", "Config-order1.R"))

# Parameter Selection  ----------------------------------------------------
# Best selection by Best found and Least number of iterations 
k1 <- 4.4
k2 <- 2.7
w <- 1.1
pop_size <- 24

base_tests <- expand.grid("k1" = k1, 
                          "k2" = k2, 
                          "w" = w, 
                          "TransFun" = baseClassTransferFunctions["S1"], 
                          "Popsize" = pop_size)
setDT(base_tests)
base_tests[, ID := .I]

# save all the results
list_results <- list()

# base parameters 
max_iter <- 30 
max_stable <- 10

total_runs <- nrow(base_tests)

ifold <- 1 

trainDT_fold <- inputDT[Fold != ifold]
testDT_fold <- inputDT[Fold == ifold]

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
                              use_var_importance = TRUE, 
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
                                       location_save = full_folder_name
                                     ), 
                                     seed = 123)
}

#TODO: vermenigvuldigen met de resultaten, dan geeft het ook weer wat echt belangrijk is en wat niet 
importances <- BPSO_swarm$get_variable_importance()
importances |> lapply(FUN = colSums) |> unlist() |> matrix(nrow = 16) |> colSums()

BPSO_run$BestResult

# this has no real value to use 
# could be caused by the inherit construction of metaheuristic algorithms
# where they perform exploration across 



