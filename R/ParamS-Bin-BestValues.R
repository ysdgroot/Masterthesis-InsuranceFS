# Loading packages and functions ------------------------------------------
source(file.path("R", "Packages.R"))

source(file.path("R", "General Parameters.R"))


# Helper functions ---------------------------------------------------------

get_save_location <- function(algo, 
                              order){
  
  save_location <- file.path("Data", 
                             "Parameters", 
                             sprintf("Best_%s_order_%d.RDS",
                                     algo,
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

# Best parameters ---------------------------------------------------------
# BAOA --------------------------------------------------------------------

## Order 1 ----------------------------------------------------------------
algo <- "BAOA"
order <- 1

dt_results_cv <- readRDS(get_file_location(algo = algo, 
                                           order = order))

best_param_run(dt_results_cv)[AvgResult == max(AvgResult)]


# best selection 
best_results_BAOA <- data.table(beta = 5, 
                                k = 0.2, 
                                minMoa = 0.3, 
                                maxMoa = 0.6, 
                                TransFunName = "V3", 
                                Popsize = 10)

save_location <- get_save_location(algo, order)

saveRDS(best_results_BAOA, 
        save_location)

## Order 2 ----------------------------------------------------------------
algo <- "BAOA"
order <- 2

dt_results_cv <- readRDS(get_file_location(algo = algo, 
                                           order = order))

best_param_run(dt_results_cv)[AvgResult == max(AvgResult)]


# best selection 
best_results_BAOA <- data.table(beta = 0, 
                                k = 0, 
                                minMoa = 0, 
                                maxMoa = 0, 
                                TransFunName = "", 
                                Popsize = 0)


save_location <- get_save_location(algo, order)

saveRDS(best_results_BAOA, 
        save_location)
# BPSO --------------------------------------------------------------------
## Order 1 ----------------------------------------------------------------
algo <- "BPSO"
order <- 1

dt_results_cv <- readRDS(get_file_location(algo = algo, 
                                           order = order))

best_param_run(dt_results_cv)[AvgResult == max(AvgResult)]


# this is some manual selection, because the results are close

# best selection 
best_results_BPSO <- data.table(k1 = 5, 
                                k2 = 2.5, 
                                w = 1.1, 
                                TransFunName = "V2", 
                                Popsize = 20)

save_location <- get_save_location(algo, order)

saveRDS(best_results_BAOA, 
        save_location)



## Order 2 ---------------------------------------------------------------
algo <- "BPSO"
order <- 2

dt_results_cv <- readRDS(get_file_location(algo = algo, 
                                           order = order))

best_param_run(dt_results_cv)[AvgResult == max(AvgResult)]

# this is some manual selection, because the results are close

# best selection 
best_results_BPSO <- data.table(k1 = 0, 
                                k2 = 0, 
                                w = 0, 
                                TransFunName = "V2", 
                                Popsize = 0)

save_location <- get_save_location(algo, order)

saveRDS(best_results_BAOA, 
        save_location)

# GA ----------------------------------------------------------------------
## Order 1 ----------------------------------------------------------------
algo <- "GA"
order <- 1

dt_results_cv <- readRDS(get_file_location(algo = algo, 
                                           order = order))

best_param_run(dt_results_cv)[AvgResult == max(AvgResult)]

# this is some manual selection, because the results are close

# best selection 
best_results_GA <- data.table(n_elits = 4, 
                              pop_size = 25, 
                              p_crossover = 0.6, 
                              p_mutation = 0.4, 
                              name_selection = "linear")


save_location <- get_save_location(algo, order)

saveRDS(best_results_BAOA, 
        save_location)


## Order 2 ----------------------------------------------------------------
algo <- "GA"
order <- 2

dt_results_cv <- readRDS(get_file_location(algo = algo, 
                                           order = order))

best_param_run(dt_results_cv)[AvgResult == max(AvgResult)]


# this is some manual selection, because the results are close

# best selection 
best_results_GA <- data.table(n_elits = 4, 
                              pop_size = 25, 
                              p_crossover = 0.6, 
                              p_mutation = 0.4, 
                              name_selection = "linear")


save_location <- get_save_location(algo, order)

saveRDS(best_results_BAOA, 
        save_location)
