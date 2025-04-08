# Loading packages and functions ------------------------------------------
source(here::here("R", "0-Packages.R"))

source(here::here("R", "1-General Parameters.R"))

# General Setup -----------------------------------------------------------

# order of the interactions
order_interaction <- 1

# Data set number - 1;2;3
data_set_number <- 2

# base name for collecting the results, %s are to be filled in with sprintf
base_name_results <- sprintf(here::here("Data", 
                                       "Parameters", 
                                       "Param_%s_DataSet%s_order%s.RDS"), 
                             "%s", 
                             data_set_number, 
                             order_interaction)

# only for Elastic Net to save lambda
data_used <- retrieve_data_set(data_set_number)


# Best parameters ---------------------------------------------------------
# GA ----------------------------------------------------------------------
algo <- "GA"

dt_results_cv <- readRDS(sprintf(base_name_results, 
                                 algo))

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

# best selection; not take roulette because it was stated that is would perform worse 
# General reason: roulette has the problem of be almost indifferent when the values are very close
best_results_GA <- data.table(n_elits = avg_best_results$AvgNelits, 
                              pop_size = avg_best_results$AvgPopsize, 
                              p_crossover = avg_best_results$AvgPcrossover, 
                              p_mutation = avg_best_results$AvgPmutation, 
                              NameSelection = "roulette")
# save the result
save_location <- get_save_location(algo = algo, 
                                   order = order_interaction, 
                                   data_set_number = data_set_number)

saveRDS(best_results_GA, 
        save_location)


# BAOA --------------------------------------------------------------------
algo <- "BAOA"

dt_results_cv <- readRDS(sprintf(base_name_results, 
                                 algo))

best_results <- best_param_run(dt_results_cv)[AvgResult == max(AvgResult)]

# continuous parameters
avg_best_results <- best_results[ ,.(AvgBeta = round(mean(beta)), 
                                     Avgk = mean(k), 
                                     AvgMinMoa = mean(minMoa), 
                                     AvgMaxMoa = mean(maxMoa),
                                     AvgPopsize = mean(Popsize))]

# categorical parameters
best_results[ ,.(Count = .N), 
              by = "TransFunName"][order(-Count)]


# this is some manual selection, because the results are close

# best selection 
best_results_BAOA <- data.table(beta = avg_best_results$AvgBeta, 
                                k = avg_best_results$Avgk, 
                                minMoa =avg_best_results$AvgMinMoa, 
                                maxMoa = avg_best_results$AvgMaxMoa, 
                                TransFunName = "V2", 
                                Popsize = avg_best_results$AvgPopsize)

# save the result
save_location <- get_save_location(algo = algo, 
                                   order = order_interaction, 
                                   data_set_number = data_set_number)

saveRDS(best_results_BAOA, 
        save_location)

# BPSO --------------------------------------------------------------------
algo <- "BPSO"

dt_results_cv <- readRDS(sprintf(base_name_results, 
                                 algo))

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
                                TransFunName = "S3", 
                                Popsize = avg_best_results$AvgPopsize)

# save the result
save_location <- get_save_location(algo = algo, 
                                   order = order_interaction, 
                                   data_set_number = data_set_number)

saveRDS(best_results_BPSO, 
        save_location)



# Elastic Net (alpha = 1): Lasso -------------------------------------------------
algo <- "Lasso"
alpha <- 1
order_interaction <- 2

best_lamba <- best_elasticnet_lambda(data_used$Data, 
                                     variables = data_used$Variables, 
                                     target_variable = data_used$Target, 
                                     distribution_model = data_used$Distribution, 
                                     alpha = alpha, 
                                     order = order_interaction, 
                                     offset = data_used$Offset)


# base name for collecting the results, %s are to be filled in with sprintf
file_location <- sprintf(here::here("Data", 
                                   "Parameters", 
                                   "Param_%s_DataSet%s_order%s.RDS"), 
                         algo, 
                         data_set_number, 
                         order_interaction)

# save the result
save_location <- get_save_location(algo = algo, 
                                   order = order_interaction, 
                                   data_set_number = data_set_number)
saveRDS(best_lamba, 
        save_location)

# Elastic Net (alpha = 0.5) -------------------------------------------------
algo <- "ElasticNet"
alpha <- 0.5
order_interaction <- 2

best_lamba <- best_elasticnet_lambda(data_used$Data, 
                                     variables = data_used$Variables, 
                                     target_variable = data_used$Target, 
                                     distribution_model = data_used$Distribution, 
                                     alpha = alpha, 
                                     order = order_interaction, 
                                     offset = data_used$Offset)


# base name for collecting the results, %s are to be filled in with sprintf
file_location <- sprintf(here::here("Data", 
                                   "Parameters", 
                                   "Param_%s_DataSet%s_order%s.RDS"), 
                         algo, 
                         data_set_number, 
                         order_interaction)

# save the result
save_location <- get_save_location(algo = algo, 
                                   order = order_interaction, 
                                   data_set_number = data_set_number)
saveRDS(best_lamba, 
        save_location)

# Elastic Net (alpha = 0): Ridge -------------------------------------------------
algo <- "ridge"
alpha <- 0
order_interaction <- 2

best_lamba <- best_elasticnet_lambda(data_used$Data, 
                                     variables = data_used$Variables, 
                                     target_variable = data_used$Target, 
                                     distribution_model = data_used$Distribution, 
                                     alpha = alpha, 
                                     order = order_interaction, 
                                     offset = data_used$Offset)


# base name for collecting the results, %s are to be filled in with sprintf
file_location <- sprintf(here::here("Data", 
                                   "Parameters", 
                                   "Param_%s_DataSet%s_order%s.RDS"), 
                         algo, 
                         data_set_number, 
                         order_interaction)

# save the result
save_location <- get_save_location(algo = algo, 
                                   order = order_interaction, 
                                   data_set_number = data_set_number)
saveRDS(best_lamba, 
        save_location)
