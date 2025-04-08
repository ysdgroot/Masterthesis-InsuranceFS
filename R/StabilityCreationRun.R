# Package loading ---------------------------------------------------------

source(here::here("R", "0-Packages.R"))
source(here::here("R", "1-General Parameters.R")) # also loads the functions

# Setup -------------------------------------------------------------------

# Data Set to be used for the parameter selection 
data_set_number <- 1

# for the continuous concordance probability
nu <- 100

# Order of interactions
order_interaction <- 2

seed <- 123

# all percentages to look at
percentages <- seq(0.8, 1, by = 0.1)
# number of runs per percentage
n_runs <- 5

# table with
## Method (GA, BAOA, BPSO, Lasso, ElasticNet, Ridge, Xgboost, forward, backward)
## Order 
## Percentage 
## RunNumber 
## VariableImportance 
## VariableSubset 
## ConcProbTrainModel
## ConcProbTestModel
## ConcProbTrainGLM
## ConcProbTestGLM


save_results <- function(results, 
                         method, 
                         order, 
                         percentage, 
                         run_number, 
                         var_importance, 
                         var_selection, 
                         conc_train_model, 
                         conc_test_model, 
                         conc_train_glm, 
                         conc_test_glm) {
  
  results <- rbindlist(list(results, 
                            data.table("Method" = method, 
                                       "Order" = order, 
                                       "RunNumber" = run_number, 
                                       "Percentage" = percentage, 
                                       "VariableImportance" = var_importance, 
                                       "VariableSubset" = var_selection,
                                       "ConcProbTrainModel" = conc_train_model, 
                                       "ConcProbTestModel" = conc_test_model, 
                                       "ConcProbTrainGLM" = conc_train_glm, 
                                       "ConcProbTestGLM" = conc_test_glm)))
  return(results)
}

# Setup Metaheuristics ----------------------------------------------------

# helper function for the best parameters
get_best_param_MH <- function(method, 
                              data_set_number, 
                              order_interaction = 1, 
                              base_location = here::here("Data", 
                                                        "Parameters", 
                                                        "Best_%s_Data%s_order_%s.RDS")) {
  
  results <- readRDS(sprintf(base_location, 
                            method, 
                            data_set_number, 
                            order_interaction))
  
  return(results)
}


##### General #########-
max_iter <- ifelse(order_interaction == 1, 30, 60)
max_stable <- ifelse(order_interaction == 1, 10, 20)
pop_size <- 25

##### GA  #########-
run_parallel <- TRUE # only for the GA run

name_selection <- c("linear", "nonlinear", "roulette", "tournament")
dt_selection <- data.table(NameSelection = name_selection, 
                           Selection = c(gabin_lrSelection, 
                                         gabin_nlrSelection, 
                                         gabin_rwSelection, 
                                         gabin_tourSelection))

params_GA <- get_best_param_MH("GA", 
                               data_set_number = data_set_number, 
                               order_interaction = 1)

selection <- dt_selection[NameSelection == params_GA$NameSelection][["Selection"]][[1]]
p_crossover <- params_GA$p_crossover
p_mutation <- params_GA$p_mutation 
n_elits <- params_GA$n_elits


##### BAOA  #########-
params_BAOA <- get_best_param_MH("BAOA", 
                                 data_set_number = data_set_number, 
                                 order_interaction = 1)

beta <- params_BAOA$beta
k <- params_BAOA$k
minMoa <- params_BAOA$minMoa
maxMoa <- params_BAOA$maxMoa
transferFun_baoa <- params_BAOA$TransFunName  

# get the Transfer Function
transferFun_baoa <- baseClassTransferFunctions[transferFun_baoa][[1]]

##### BPSO #########-
params_BPSO <- get_best_param_MH("BPSO", 
                                 data_set_number = data_set_number, 
                                 order_interaction = 1)

k1 <- params_BPSO$k1
k2 <- params_BPSO$k2
w <- params_BPSO$w
transferFun_bpso <- params_BPSO$TransFunName

# get the Transfer Function
transferFun_bpso <- baseClassTransferFunctions[transferFun_bpso][[1]]

# Setup Elastic Net -------------------------------------------------------

# helper function to retrieve the lambdas for the Elastic Net methods
get_lamba <- function(method, 
                      data_set_number, 
                      order_interaction, 
                      base_location_lambda = here::here("Data", 
                                                       "Parameters", 
                                                       "Best_%s_Data%s_order_%s.RDS")){
  
  lambda <- readRDS(sprintf(base_location_lambda, 
                            method, 
                            data_set_number, 
                            order_interaction))
  
  # take the average of both -- Not to be to data dependend 
  lambda <- mean(c(lambda$MinLambda,
                   lambda$MaxLambda))
  
  return(lambda)
}


# Elastic Net: alpha = 1 (Lasso)
lambda_lasso <- get_lamba("Lasso", 
                          data_set_number = data_set_number, 
                          order_interaction = order_interaction)

# Elastic Net: alpha = 0.5 (Elastic Net)
lambda_elasticnet <- get_lamba("ElasticNet", 
                               data_set_number = data_set_number, 
                               order_interaction = order_interaction)

# Elastic Net: alpha = 0 (Ridge)
lambda_ridge <- get_lamba("ridge", 
                          data_set_number = data_set_number, 
                          order_interaction = order_interaction)

# Data Loading ------------------------------------------------------------

# import data set
data_import <- retrieve_data_set(data_set_number = data_set_number)

data <- data_import[["Data"]]
variables <- data_import[["Variables"]]
target_variable <- data_import[["Target"]]
offset <- data_import[["Offset"]]
concProb_type <- data_import[["ConcProbType"]]
distribution_model <- data_import[["Distribution"]]

# remove unnecessary information
rm(data_import)

# Setup XGBoost -----------------------------------------------------------

objective <- ifelse(distribution_model$family == "Gamma", "reg:gamma", "count:poisson") 
booster <- "gbtree"
nrounds <- ifelse(order_interaction == 1, 30, 60)

# -------------------------------------------------------------------------

# file to save the results to after each run 

file_name_save <- here::here("Data", 
                            "StabilityRun", 
                            sprintf("Data_%s_order_%s.RDS", 
                                    data_set_number, 
                                    order_interaction))

results <- data.table()

if (!file.exists(file_name_save)) {
  saveRDS(data.table(), 
          file = file_name_save)
} else {
  results <- readRDS(file = file_name_save)
}


# Start Tests -------------------------------------------------------------

count_tests <- length(percentages) * n_runs
i <- 0
for (perc in percentages) {
  for (i_run in 1:n_runs) {
    i <- i + 1
    cat(red(sprintf("Test [%s/%s] - Percentage [%s] - Run [%s/%s] \n", 
                    i, 
                    count_tests, 
                    perc, 
                    i_run, 
                    n_runs)))
    
    # temporary location to save some results 
    folder_name_glm <- here::here("Data", 
                              sprintf("Data%sorder%perc%srun%s", 
                                      data_set_number, 
                                      order_interaction, 
                                      perc, 
                                      i_run))
    
    if (!dir.exists(folder_name_glm)) {dir.create(folder_name_glm)}
    
    
    # Creation of data sets 
    # use the function, set seed for reproducibility
    ls_data <- partial_resample_data(data, 
                                     perc_value = perc, 
                                     seed = seed + 2*perc + i_run)
    
    train_dt <- ls_data$Train
    test_dt <- ls_data$Test
    
    ## START RUNS
    # Run for GA ######################################################
    
    cat(sprintf("\t 1-GA \n"))

    result_run <- run_GA(train = train_dt,
                         test = test_dt,
                         variables = variables,
                         target_variable = target_variable,
                         distribution_model = distribution_model,
                         order = order_interaction,
                         offset = offset,
                         type = concProb_type,
                         location_glm_results = folder_name_glm,
                         parallel = run_parallel,
                         nu = nu,
                           selection = selection,
                           p_crossover = p_crossover,
                           p_mutation = p_mutation,
                           n_elits = n_elits,
                           pop_size = pop_size,
                           max_stable = max_stable,
                           max_iter = max_iter)

    # store the results
    results <- save_results(results,
                            method = "GA",
                            order = order_interaction,
                            percentage = perc,
                            run_number = i_run,
                            var_importance = result_run["VariableImportance"],
                            var_selection = result_run["VariableSubset"] ,
                            conc_train_model= result_run["ConcProbTrainModel"],
                            conc_test_model= result_run["ConcProbTestModel"],
                            conc_train_glm = result_run["ConcProbTrainGLM"],
                            conc_test_glm = result_run["ConcProbTestGLM"])
    
    saveRDS(results, 
            file = file_name_save) 
    
    # Run for BAOA ######################################################
    
    cat(sprintf("\t 2-BAOA \n"))
    
    result_run <- run_BAOA(train = train_dt, 
                         test = test_dt, 
                         variables = variables, 
                         target_variable = target_variable, 
                         distribution_model = distribution_model, 
                         order = order_interaction, 
                         offset = offset, 
                         type = concProb_type, 
                         location_glm_results = folder_name_glm, 
                         nu = nu, 
                           beta = beta,
                           k = k, 
                           minMoa = minMoa, 
                           maxMoa = maxMoa, 
                           transferFun = transferFun_baoa, 
                           pop_size = pop_size, 
                           max_stable = max_stable, 
                           max_iter = max_iter)
    
    # store the results 
    results <- save_results(results, 
                            method = "BAOA", 
                            order = order_interaction, 
                            percentage = perc, 
                            run_number = i_run, 
                            var_importance = result_run["VariableImportance"], 
                            var_selection = result_run["VariableSubset"] , 
                            conc_train_model= result_run["ConcProbTrainModel"], 
                            conc_test_model= result_run["ConcProbTestModel"], 
                            conc_train_glm = result_run["ConcProbTrainGLM"], 
                            conc_test_glm = result_run["ConcProbTestGLM"])
    
    saveRDS(results, 
            file = file_name_save) 
    
    # Run for BPSO ######################################################
    
    cat(sprintf("\t 3-BPSO \n"))
    
    result_run <- run_BPSO(train = train_dt, 
                           test = test_dt, 
                           variables = variables, 
                           target_variable = target_variable, 
                           distribution_model = distribution_model,
                           order = order_interaction, 
                           offset = offset, 
                           type = concProb_type, 
                           location_glm_results = folder_name_glm, 
                           nu = nu, 
                             k1 = k1 , 
                             k2 = k2, 
                             w = w, 
                             transferFun = transferFun_baoa, 
                             pop_size = pop_size, 
                             max_stable = max_stable, 
                             max_iter = max_iter)
    
    # store the results 
    results <- save_results(results, 
                            method = "BPSO", 
                            order = order_interaction, 
                            percentage = perc, 
                            run_number = i_run, 
                            var_importance = result_run["VariableImportance"], 
                            var_selection = result_run["VariableSubset"] , 
                            conc_train_model= result_run["ConcProbTrainModel"], 
                            conc_test_model= result_run["ConcProbTestModel"], 
                            conc_train_glm = result_run["ConcProbTrainGLM"], 
                            conc_test_glm = result_run["ConcProbTestGLM"])
    
    saveRDS(results, 
            file = file_name_save) 
    
    # Run for Elastic Net (alpha = 0) Ridge ######################################################
    
    cat(sprintf("\t 4-Ridge \n"))
    
    result_run <- run_elasticNet(train = train_dt, 
                                 test = test_dt, 
                                 variables = variables, 
                                 target_variable = target_variable, 
                                 distribution_model = distribution_model,
                                 order = order_interaction, 
                                 offset = offset, 
                                 type = concProb_type, 
                                 location_glm_results = folder_name_glm, 
                                 nu = nu, 
                                  alpha = 0, 
                                  lambda = lambda_ridge)
    
    # store the results 
    results <- save_results(results, 
                            method = "Ridge", 
                            order = order_interaction, 
                            percentage = perc, 
                            run_number = i_run, 
                            var_importance = result_run["VariableImportance"], 
                            var_selection = result_run["VariableSubset"] , 
                            conc_train_model= result_run["ConcProbTrainModel"], 
                            conc_test_model= result_run["ConcProbTestModel"], 
                            conc_train_glm = result_run["ConcProbTrainGLM"], 
                            conc_test_glm = result_run["ConcProbTestGLM"])
    
    saveRDS(results, 
            file = file_name_save) 
    
    # Run for Elastic Net (alpha = 0.5) - Elastic Net ######################################################
    
    cat(sprintf("\t 5-Elastic Net \n"))
    
    result_run <- run_elasticNet(train = train_dt, 
                                 test = test_dt, 
                                 variables = variables, 
                                 target_variable = target_variable, 
                                 distribution_model = distribution_model,
                                 order = order_interaction, 
                                 offset = offset, 
                                 type = concProb_type, 
                                 location_glm_results = folder_name_glm, 
                                 nu = nu, 
                                   alpha = 0.5, 
                                   lambda = lambda_elasticnet)
    
    # store the results 
    results <- save_results(results, 
                            method = "ElasticNet", 
                            order = order_interaction, 
                            percentage = perc, 
                            run_number = i_run, 
                            var_importance = result_run["VariableImportance"], 
                            var_selection = result_run["VariableSubset"] , 
                            conc_train_model= result_run["ConcProbTrainModel"], 
                            conc_test_model= result_run["ConcProbTestModel"], 
                            conc_train_glm = result_run["ConcProbTrainGLM"], 
                            conc_test_glm = result_run["ConcProbTestGLM"])
    
    saveRDS(results, 
            file = file_name_save) 
    
    # Run for Elastic Net (alpha = 1) - Lasso ######################################################
    
    cat(sprintf("\t 6-Lasso \n"))
    
    result_run <- run_elasticNet(train = train_dt, 
                                 test = test_dt, 
                                 variables = variables, 
                                 target_variable = target_variable, 
                                 distribution_model = distribution_model,
                                 order = order_interaction, 
                                 offset = offset, 
                                 type = concProb_type, 
                                 location_glm_results = folder_name_glm, 
                                 nu = nu, 
                                   alpha = 1, 
                                   lambda = lambda_lasso)
    
    # store the results 
    results <- save_results(results, 
                            method = "Lasso", 
                            order = order_interaction, 
                            percentage = perc, 
                            run_number = i_run, 
                            var_importance = result_run["VariableImportance"], 
                            var_selection = result_run["VariableSubset"] , 
                            conc_train_model= result_run["ConcProbTrainModel"], 
                            conc_test_model= result_run["ConcProbTestModel"], 
                            conc_train_glm = result_run["ConcProbTrainGLM"], 
                            conc_test_glm = result_run["ConcProbTestGLM"])
    
    saveRDS(results, 
            file = file_name_save) 
    
    # Run for XGBoost ######################################################
    
    cat(sprintf("\t 7-XGBoost \n"))
    
    result_run <- run_xgboost(train = train_dt, 
                             test = test_dt, 
                             variables = variables, 
                             target_variable = target_variable, 
                             distribution_model = distribution_model, 
                             order = order_interaction, 
                             offset = offset, 
                             type = concProb_type, 
                             location_glm_results = folder_name_glm,  
                             nu = nu, 
                               booster = booster, 
                               objective = objective, 
                               nrounds = nrounds)
    
    # store the results 
    results <- save_results(results, 
                            method = "XGBoost", 
                            order = order_interaction, 
                            percentage = perc, 
                            run_number = i_run, 
                            var_importance = result_run["VariableImportance"], 
                            var_selection = result_run["VariableSubset"] , 
                            conc_train_model= result_run["ConcProbTrainModel"], 
                            conc_test_model= result_run["ConcProbTestModel"], 
                            conc_train_glm = result_run["ConcProbTrainGLM"], 
                            conc_test_glm = result_run["ConcProbTestGLM"])
    
    saveRDS(results, 
            file = file_name_save) 
    # Run for Forward ######################################################
    
    cat(sprintf("\t 8-Forward \n"))
    
    result_run <- run_stepwise(train = train_dt, 
                              test = test_dt, 
                              variables = variables, 
                              target_variable = target_variable, 
                              distribution_model = distribution_model,
                              order = order_interaction, 
                              offset = offset, 
                              type = concProb_type, 
                              location_glm_results = folder_name_glm,
                              nu = nu, 
                                type_run = "forward", 
                                is_minimize = FALSE)
    
    # store the results 
    results <- save_results(results, 
                            method = "Forward", 
                            order = order_interaction, 
                            percentage = perc, 
                            run_number = i_run, 
                            var_importance = result_run["VariableImportance"], 
                            var_selection = result_run["VariableSubset"] , 
                            conc_train_model= result_run["ConcProbTrainModel"], 
                            conc_test_model= result_run["ConcProbTestModel"], 
                            conc_train_glm = result_run["ConcProbTrainGLM"], 
                            conc_test_glm = result_run["ConcProbTestGLM"])
    
    # Run for Backward ######################################################
    
    # cat(sprintf("\t 9-Backward \n"))
    # 
    # result_run <- run_stepwise(train = train_dt, 
    #                            test = test_dt, 
    #                            variables = variables, 
    #                            target_variable = target_variable, 
    #                            distribution_model = distribution_model,
    #                            order = order_interaction, 
    #                            offset = offset, 
    #                            type = concProb_type, 
    #                            location_glm_results = folder_name_glm,
    #                            nu = nu, 
    #                              type_run = "backward", 
    #                              is_minimize = FALSE)
    # 
    # # store the results 
    # results <- save_results(results, 
    #                         method = "Backward", 
    #                         order = order_interaction, 
    #                         percentage = perc, 
    #                         run_number = i_run, 
    #                         var_importance = result_run["VariableImportance"], 
    #                         var_selection = result_run["VariableSubset"] , 
    #                         conc_train_model= result_run["ConcProbTrainModel"], 
    #                         conc_test_model= result_run["ConcProbTestModel"], 
    #                         conc_train_glm = result_run["ConcProbTrainGLM"], 
    #                         conc_test_glm = result_run["ConcProbTestGLM"])
    
    
  saveRDS(results, 
          file = file_name_save)  
  } # end run numbers
} # end run percentages



