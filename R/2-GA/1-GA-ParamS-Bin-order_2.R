# Importing library -------------------------------------------------------

source(file.path("R", "0-Packages.R"))

source(file.path("R", "1-General Parameters.R"))
source(file.path("R", "1-Config-order2.R"))

# Parameter selection is based on the best results of order 1
# otherwise to much compute time 

# Parameter Selection  ----------------------------------------------------
# number of elits 
# population size 
####### selection
# Linear-rank selection -- r and q
# Nonlinear-rank selection -- q
# roulette wheel --
# Tournament Selection -- k = 3

####### crossover
# Uniform crossover 

####### Mutation 
# Uniform-random --> probability

n_elits <- 4
pop_size <- seq(20, 25, 5)

p_crossover <- seq(0.5, 0.7, 0.1)
p_mutation <- seq(0.3, 0.5, 0.1)

name_selection <- c("linear")
dt_selection <- data.table(NameSelection = c("linear", "nonlinear", "roulette", "tournament"), 
                           Selection = c(gabin_lrSelection, 
                                         gabin_nlrSelection, 
                                         gabin_rwSelection, 
                                         gabin_tourSelection))

base_tests <- expand.grid("n_elits" = n_elits, 
                          "pop_size" = pop_size, 
                          "p_crossover" = p_crossover, 
                          "p_mutation" = p_mutation, 
                          "NameSelection" = name_selection)
setDT(base_tests)

base_tests <- base_tests |> 
  collapse::join(dt_selection, 
                 on = "NameSelection", 
                 how = "left")
base_tests[, ID := .I]

# save all the results
list_results <- list()

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
    
    GA_sim <- ga(fitness = mfitness, 
                       trainDT = trainDT, 
                       testDT = testDT, 
                       variableHandler = VH, 
                       targetVar = "claimNumber", 
                       distMod = poisson(link='log'), 
                       offset = "exposure", 
                       nullValue = 0.5,
                       withMain = withMain,
                       location_save = full_folder_name,
                   type = "binary", # optimization data type
                   population = gabin_Population,
                   suggestions = NULL,
                   selection = base_tests[i,]$Selection[[1]], 
                   crossover = gabin_uCrossover,  # cross-over method
                   pcrossover = base_tests[i,]$p_crossover,
                   mutation = gabin_raMutation, # uniform random 
                   pmutation = base_tests[i,]$p_mutation, 
                   elitism = base_tests[i,]$n_elits, # best N indiv. to pass to next iteration
                   popSize = base_tests[i,]$pop_size, # number of the population in each evolution
                   nBits = VH$get_length(), # total number of variables
                   names = VH$get_used_variables(), # variable name
                   run = max_stable, # max iter without improvement (stopping criteria)
                   maxiter = max_iter, # total runs or generations
                   monitor = FALSE, 
                   keepBest = TRUE, # keep the best solution at the end
                   parallel = TRUE,  
                   seed=9876)
    
    
    # save the results 
    
    result <- data.table("nIterations" = GA_sim@iter, 
                         "BestResult" = GA_sim@fitnessValue, 
                         "ID" = i, 
                         "Fold" = ifold)
    cat(green(sprintf("ID: %d - nIterations: %d - Result: %g - Position: %s \n", 
                      i, 
                      GA_sim@iter, 
                      GA_sim@fitnessValue, 
                      paste(GA_sim@bestSol[[length(GA_sim@bestSol)]], collapse = ""))))
    
    list_results <- append(list_results, list(result))
  }
}

dt_results_cv_ga <- rbindlist(list_results)

dir.create(file.path("Data", 
                     "Parameters"), 
           showWarnings = FALSE)

#Use the name of the Transfer function, rather than the object
base_tests_temp <- copy(base_tests)
base_tests_temp[, Selection := NULL]

dt_results_cv_ga <- dt_results_cv_ga |> 
  collapse::join(base_tests_temp, 
                 on = 'ID', 
                 how = "left")

saveRDS(dt_results_cv_ga, 
        sprintf(file.path("Data", 
                          "Parameters", 
                          "Param_GA_bin_order%d.RDS"), 
                order))

