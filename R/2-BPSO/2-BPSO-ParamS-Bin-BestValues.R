# Loading packages and functions ------------------------------------------
source(file.path("R", "0-Packages.R"))

source(file.path("R", "1-General Parameters.R"))

# Best parameters ---------------------------------------------------------
# BPSO --------------------------------------------------------------------
## Order 1 ----------------------------------------------------------------
algo <- "BPSO"
order <- 1

dt_results_cv <- readRDS(get_file_location(algo = algo, 
                                           order = order))

best_results <- best_param_run(dt_results_cv)[AvgResult == max(AvgResult)]

# continuous parameters
avg_best_results <- best_results[ ,.(Avgk1 = mean(k1), 
                                     Avgk2 = mean(k2), 
                                     Avgw = mean(w), 
                                     AvgPopsize = mean(Popsize))]

# categorical parameters
best_results[ ,.(Count = .N), 
              by = "TransFunName"][order(-Count)]

# this is some manual selection, because the results are close

# best selection 
best_results_BPSO <- data.table(k1 = avg_best_results$Avgk1, 
                                k2 = avg_best_results$Avgk2, 
                                w = avg_best_results$Avgw, 
                                TransFunName = "V2", 
                                Popsize = avg_best_results$AvgPopsize)

save_location <- get_save_location(algo, order)

saveRDS(best_results_BAOA, 
        save_location)

## Order 2 ---------------------------------------------------------------
algo <- "BPSO"
order <- 2

dt_results_cv <- readRDS(get_file_location(algo = algo, 
                                           order = order))

best_results <- best_param_run(dt_results_cv)[AvgResult == max(AvgResult)]

# continuous parameters
avg_best_results <- best_results[ ,.(Avgk1 = mean(k1), 
                                     Avgk2 = mean(k2), 
                                     Avgw = mean(w), 
                                     AvgPopsize = mean(Popsize))]

# categorical parameters
best_results[ ,.(Count = .N), 
              by = "TransFunName"][order(-Count)]

# this is some manual selection, because the results are close

# best selection 
best_results_BPSO <- data.table(k1 = avg_best_results$Avgk1, 
                                k2 = avg_best_results$Avgk2, 
                                w = avg_best_results$Avgw, 
                                TransFunName = "V2", 
                                Popsize = avg_best_results$AvgPopsize)

save_location <- get_save_location(algo, order)

saveRDS(best_results_BAOA, 
        save_location)

