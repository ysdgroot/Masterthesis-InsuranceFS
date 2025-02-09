# Loading packages and functions ------------------------------------------
source(file.path("R", "Packages.R"))

source(file.path("R", "General Parameters.R"))


# Best parameters ---------------------------------------------------------
# BAOA --------------------------------------------------------------------

## Order 1 ----------------------------------------------------------------
algo <- "BAOA"
order <- 1

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

# GA ----------------------------------------------------------------------
## Order 1 ----------------------------------------------------------------
algo <- "GA"
order <- 1

dt_results_cv <- readRDS(get_file_location(algo = algo, 
                                           order = order))

best_results <- best_param_run(dt_results_cv)[AvgResult == max(AvgResult)]

# continuous parameters
avg_best_results <- best_results[ ,.(AvgNelits = round(mean(n_elits)), 
                                     AvgPcrossover = mean(p_crossover), 
                                     AvgPmutation = mean(p_mutation), 
                                     AvgPopsize = mean(pop_size))]

# categorical parameters
best_results[ ,.(Count = .N), 
              by = "NameSelection"][order(-Count)]


# this is some manual selection, because the results are close

# best selection 
best_results_GA <- data.table(n_elits = avg_best_results$AvgNelits, 
                              pop_size = avg_best_results$AvgPopsize, 
                              p_crossover = avg_best_results$AvgPcrossover, 
                              p_mutation = avg_best_results$AvgPmutation, 
                              NameSelection = "linear")


save_location <- get_save_location(algo, order)

saveRDS(best_results_BAOA, 
        save_location)


## Order 2 ----------------------------------------------------------------
algo <- "GA"
order <- 2

dt_results_cv <- readRDS(get_file_location(algo = algo, 
                                           order = order))

best_results <- best_param_run(dt_results_cv)[AvgResult == max(AvgResult)]

# continuous parameters
avg_best_results <- best_results[ ,.(AvgNelits = round(mean(n_elits)), 
                                     AvgPcrossover = mean(p_crossover), 
                                     AvgPmutation = mean(p_mutation), 
                                     AvgPopsize = mean(pop_size))]

# categorical parameters
best_results[ ,.(Count = .N), 
              by = "NameSelection"][order(-Count)]


# this is some manual selection, because the results are close

# best selection 
best_results_GA <- data.table(n_elits = avg_best_results$AvgNelits, 
                              pop_size = avg_best_results$AvgPopsize, 
                              p_crossover = avg_best_results$AvgPcrossover, 
                              p_mutation = avg_best_results$AvgPmutation, 
                              NameSelection = "linear")


save_location <- get_save_location(algo, order)

saveRDS(best_results_BAOA, 
        save_location)
