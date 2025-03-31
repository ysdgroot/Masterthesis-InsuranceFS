# Loading packages and functions ------------------------------------------
source(file.path("R", "0-Packages.R"))

source(file.path("R", "1-General Parameters.R"))

# Best parameters ---------------------------------------------------------
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

saveRDS(best_results_GA, 
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

saveRDS(best_results_GA, 
        save_location)
