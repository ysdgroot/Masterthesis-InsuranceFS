# Loading packages and functions ------------------------------------------
source(file.path("R", "0-Packages.R"))
source(file.path("R", "1-General Parameters.R"))

# Best parameters ---------------------------------------------------------
## Order 1 ----------------------------------------------------------------
algo <- "BAOA"
order <- 1

dt_results_cv <- readRDS(get_file_location(algo = algo, 
                                           order = order))

best_results <- best_param_run(dt_results_cv)[AvgResult == max(AvgResult)]

# continuous parameters
avg_best_results <- best_results[ ,.(AvgBeta =   mean(beta), 
                                      Avgk = mean(k), 
                                      AvgMinMoa = mean(minMoa), 
                                      AvgMaxMoa = mean(maxMoa),
                                      AvgPopsize = mean(Popsize))]

# categorical parameters
best_results[ ,.(Count = .N), 
              by = "TransFunName"][order(-Count)]

# best selection 
best_results_BAOA <- data.table(beta = avg_best_results$AvgBeta, 
                                k = avg_best_results$Avgk, 
                                minMoa =avg_best_results$AvgMinMoa, 
                                maxMoa = avg_best_results$AvgMaxMoa, 
                                TransFunName = "V3", 
                                Popsize = avg_best_results$AvgPopsize)

save_location <- get_save_location(algo, order)

saveRDS(best_results_BAOA, 
        save_location)

## Order 2 ----------------------------------------------------------------
algo <- "BAOA"
order <- 2

dt_results_cv <- readRDS(get_file_location(algo = algo, 
                                           order = order))

best_results <- best_param_run(dt_results_cv)[AvgResult == max(AvgResult)]

# continuous parameters
avg_best_results <- best_results[ ,.(AvgBeta = mean(beta), 
                                     Avgk = mean(k), 
                                     AvgMinMoa = mean(minMoa), 
                                     AvgMaxMoa = mean(maxMoa),
                                     AvgPopsize = mean(Popsize))]

# categorical parameters
best_results[ ,.(Count = .N), 
              by = "TransFunName"][order(-Count)]

# best selection 
best_results_BAOA <- data.table(beta = avg_best_results$AvgBeta, 
                                k = avg_best_results$Avgk, 
                                minMoa =avg_best_results$AvgMinMoa, 
                                maxMoa = avg_best_results$AvgMaxMoa, 
                                TransFunName = "V3", 
                                Popsize = avg_best_results$AvgMaxMoa)


save_location <- get_save_location(algo, order)

saveRDS(best_results_BAOA, 
        save_location)

