
# Package loading ---------------------------------------------------------

source(here::here("R", "0-Packages.R"))
source(here::here("R", "1-General Parameters.R"))


# Data Sets loading -------------------------------------------------------

#source(here::here("R", "1-DataSets.R"))

# Setup -------------------------------------------------------------------

run_GA_parameter_selection <-  TRUE
run_BAOA_parameter_selection <- FALSE
run_BPSO_parameter_selection <- FALSE

# Data Set to be used for the parameter selection 
data_set_number <- 1

# for the continuous concordance probability
nu <- 100

# Not test order 2, because it takes to long to test
order_interaction <- 1

# standard values across methods 
max_iter <- 30
max_stable <- 10

seed <- 42
nfolds <- 5
run_parallel <- FALSE # only for the GA run

base_name_folder <- here::here("Data", 
                               "ForParameterSelection", 
                              "DataSet%s_Fold%s_order%s")
base_name_folder <- sprintf(base_name_folder, 
                            data_set_number, 
                            "%s", 
                            order_interaction)

# Import Data Set ---------------------------------------------------------

# import data set
data_import <- retrieve_data_set(data_set_number = data_set_number)

data <- data_import[["Data"]]
variables <- data_import[["Variables"]]
target_variable <- data_import[["Target"]]
offset <- data_import[["Offset"]]
concProb_type <- data_import[["ConcProbType"]]
distribution_model <- data_import[["Distribution"]]

# Parameters: GA ----------------------------------------------------------------------
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

n_elits <- 2:5
pop_size <- 25

p_crossover <- seq(0.5, 0.9, 0.1)
p_mutation <- seq(0.1, 0.5, 0.1)

name_selection <- c("linear", "nonlinear", "roulette", "tournament")
dt_selection <- data.table(NameSelection = name_selection, 
                           Selection = c(gabin_lrSelection, 
                                         gabin_nlrSelection, 
                                         gabin_rwSelection, 
                                         gabin_tourSelection))

base_tests_ga <- expand.grid("n_elits" = n_elits,
                             "pop_size" = pop_size,
                             "p_crossover" = p_crossover,
                             "p_mutation" = p_mutation,
                             "NameSelection" = name_selection)

# base_tests_ga <- expand.grid("n_elits" = 3,
#                              "pop_size" = pop_size,
#                              "p_crossover" = 0.5,
#                              "p_mutation" = 0.3,
#                              "NameSelection" = name_selection)

setDT(base_tests_ga)

base_tests_ga <- base_tests_ga |> 
  collapse::join(dt_selection, 
                 on = "NameSelection", 
                 how = "left")
base_tests_ga[, ID := .I]

# Selection: GA --------------------------------------------------------------------

# run if set up
if (run_GA_parameter_selection) {
  
  cat(crayon::red("Start GA modelling \n"))
  
  list_results <- list()
  
  total_runs <- nrow(base_tests_ga)
  for (ifold in 1:nfolds) {
    cat(crayon::blue(sprintf("Start Fold: %d ---------------- \n", 
                             ifold)))
    
    trainDT_fold <- data[Fold != ifold]
    testDT_fold <- data[Fold == ifold]
    
    # create folder to save results 
    folder_name <- sprintf(base_name_folder, 
                           ifold)
    dir.create(folder_name, showWarnings = FALSE)
    
    for (i in 1:nrow(base_tests_ga)) {
      cat(crayon::blue(sprintf("Fold: %d \t Run: %d/%d \n", 
                               ifold, 
                               i, 
                               total_runs)))
      
      # collect the parameters
      pop_size <- base_tests_ga[i,]$pop_size
      p_crossover <- base_tests_ga[i,]$p_crossover
      p_mutation <- base_tests_ga[i,]$p_mutation
      n_elits <- base_tests_ga[i,]$n_elits
      selection <- base_tests_ga[i,]$Selection[[1]]
      
      VH <- VariableHandler$new(variables = variables, 
                                order = order_interaction)
      
      # run the GA method 
      result_run <- run_GA(train = trainDT_fold, 
                           test = testDT_fold, 
                           variables = variables, 
                           target_variable = target_variable, 
                           distribution_model = distribution_model, 
                           selection = selection, 
                           p_crossover = p_crossover,  
                           p_mutation = p_mutation, 
                           n_elits = n_elits, 
                           pop_size = pop_size, 
                           max_stable = max_stable, 
                           max_iter = max_iter, 
                           order = order_interaction, 
                           offset = offset, 
                           concProb_type = concProb_type, 
                           location_glm_results = folder_name, 
                           parallel = run_parallel)
      
      
      
      # save the results 
      #number of iterations
      niter <- max(result_run[["AdditionalInfo"]][["Iteration"]])
      
      # put everything into a data.table
      result <- data.table("nIterations" = niter, 
                           "BestResult" = result_run[["ConcProbTestGLM"]], 
                           "ID" = i, 
                           "Fold" = ifold)
      list_results <- append(list_results, list(result))
      
      # in the data frame the position can be there multiple times --> unique
      # if multiple different coding have the same result --> take first one
      best_position <- unique(result_run[["AdditionalInfo"]][ConcProbTestGLM == result_run[["ConcProbTestGLM"]]][["Position"]])[1]
      
      # give some output 
      cat(green(sprintf("ID: %d - nIterations: %d - Result: %g - Position: %s \n", 
                        i, 
                        niter, 
                        result_run[["ConcProbTestGLM"]], 
                        best_position)))
      
    }
    
    # temporary store results
    saveRDS(rbindlist(list_results), 
            sprintf(here::here("Data", 
                              "Parameters", 
                              "Param_GA_DataSet%s_order%s_Fold%s.RDS"), 
                    data_set_number, 
                    order_interaction, 
                    ifold))
  }
  
  dt_results_cv_ga <- rbindlist(list_results)
  
  dir.create(here::here("Data", 
                       "Parameters"), 
             showWarnings = FALSE)
  
  #Use the name of the Transfer function, rather than the object
  base_tests_temp <- copy(base_tests_ga)
  base_tests_temp[, Selection := NULL]
  
  dt_results_cv_ga <- dt_results_cv_ga |> 
    collapse::join(base_tests_temp, 
                   on = 'ID', 
                   how = "left")
  
  saveRDS(dt_results_cv_ga, 
          sprintf(here::here("Data", 
                            "Parameters", 
                            "Param_GA_DataSet%s_order%s.RDS"),
                  data_set_number, 
                  order_interaction))
}

# Parameters: BAOA --------------------------------------------------------------------

# Best selection by Best found and Least number of iterations 
beta <- seq(4.9, 5.1, 0.05)
k <- seq(0.2, 0.8, 0.1)
minMoa <- seq(0.1, 0.4, 0.1)
maxMoa <- seq(0.6, 0.9, 0.1)
pop_size <- 25

base_tests_baoa <- expand.grid("beta" = beta,
                               "k" = k,
                               "minMoa" = minMoa,
                               "maxMoa" = maxMoa,
                               "TransFun" = baseClassTransferFunctions,
                               "Popsize" = pop_size)

# base_tests_baoa <- expand.grid("beta" = 5, 
#                                "k" = 0.5, 
#                                "minMoa" = 0.1, 
#                                "maxMoa" = 0.6, 
#                                "TransFun" = baseClassTransferFunctions, 
#                                "Popsize" = pop_size)

setDT(base_tests_baoa)
base_tests_baoa[, ID := .I]
# Selection: BAOA --------------------------------------------------------------------

if (run_BAOA_parameter_selection) {
  
  cat(crayon::red("Start BAOA modelling \n"))
  
  # save all the results
  list_results <- list()
  
  total_runs <- nrow(base_tests_baoa)
  
  for (ifold in 1:nfolds) {
    cat(crayon::blue(sprintf("Start Fold: %d ------------------------------------------------  \n", 
                             ifold)))
    
    trainDT_fold <- data[Fold != ifold]
    testDT_fold <- data[Fold == ifold]
    
    # create folder to save results 
    folder_name <- sprintf(base_name_folder, 
                           ifold)
    dir.create(folder_name, showWarnings = FALSE)
    
    for (i in 1:nrow(base_tests_baoa)) {
      cat(crayon::blue(sprintf("Fold: %d \t Run: %d/%d \n", 
                               ifold, 
                               i, 
                               total_runs)))
      
      # collect the parameters 
      pop_size <- base_tests_baoa[i,]$Popsize
      beta <- base_tests_baoa[i,]$beta
      k <- base_tests_baoa[i,]$k
      minMoa <- base_tests_baoa[i,]$minMoa
      maxMoa <- base_tests_baoa[i,]$maxMoa 
      transFun <- base_tests_baoa[i,]$TransFun[[1]]
      
      # run the BAOA method 
      result_run <- run_BAOA(train = trainDT_fold, 
                             test = testDT_fold, 
                             variables = variables, 
                             target_variable = target_variable, 
                             distribution_model = distribution_model, 
                             beta = beta, 
                             k = k, 
                             minMoa = minMoa, 
                             maxMoa = maxMoa, 
                             transferFun = transFun, 
                             pop_size = pop_size, 
                             max_stable = max_stable, 
                             max_iter = max_iter, 
                             order = order_interaction, 
                             offset = offset, 
                             concProb_type = concProb_type, 
                             location_glm_results = folder_name)
      
      # save the results 
      #number of iterations
      niter <- max(result_run[["AdditionalInfo"]][["Iteration"]])
      
      # put everything into a data.table
      result <- data.table("nIterations" = niter, 
                           "BestResult" = result_run[["ConcProbTestGLM"]], 
                           "ID" = i, 
                           "Fold" = ifold)
      list_results <- append(list_results, list(result))
      
      # in the data frame the position can be there multiple times --> unique
      # if multiple different coding have the same result --> take first one
      best_position <- unique(result_run[["AdditionalInfo"]][ConcProbTestGLM == result_run[["ConcProbTestGLM"]]][["Position"]])[1]
      
      # give some output 
      cat(green(sprintf("ID: %d - nIterations: %d - Result: %g - Position: %s \n", 
                        i, 
                        niter, 
                        result_run[["ConcProbTestGLM"]], 
                        best_position)))
    }
    
    # temporary store results
    saveRDS(rbindlist(list_results), 
            sprintf(here::here("Data", 
                              "Parameters", 
                              "Param_BAOA_DataSet%s_order%s_Fold%s.RDS"), 
                    data_set_number, 
                    order_interaction, 
                    ifold))
    
  }
  
  dt_results_cv_baoa <- rbindlist(list_results)
  
  dir.create(here::here("Data", 
                       "Parameters"), 
             showWarnings = FALSE)
  
  #Use the name of the Transfer function, rather than the object
  base_tests_temp <- copy(base_tests_baoa)
  base_tests_temp[, TransFunName := as.character(lapply(TransFun, \(x) x$get_name()))]
  base_tests_temp[, TransFun := NULL]
  
  dt_results_cv_baoa <- dt_results_cv_baoa |> 
    collapse::join(base_tests_temp, 
                   on = 'ID', 
                   how = "left")
  
  saveRDS(dt_results_cv_baoa, 
          sprintf(here::here("Data", 
                            "Parameters", 
                            "Param_BAOA_DataSet%s_order%s.RDS"), 
                  data_set_number, 
                  order_interaction))
}


# Parameters: BPSO --------------------------------------------------------------------
# Best selection by Best found and Least number of iterations 
k1 <- seq(1, 6, 0.5) 
k2 <- seq(1, 6, 0.5)
w <- seq(0.1, 2.1, 0.5)
pop_size <- 25

base_tests_bpso <- expand.grid("k1" = k1,
                               "k2" = k2,
                               "w" = w,
                               "TransFun" = baseClassTransferFunctions,
                               "Popsize" = pop_size)

# base_tests_bpso <- expand.grid("k1" = 3,
#                                "k2" = 3,
#                                "w" = 1,
#                                "TransFun" = baseClassTransferFunctions,
#                                "Popsize" = pop_size)

setDT(base_tests_bpso)
base_tests_bpso[, ID := .I]

# Selection: BPSO --------------------------------------------------------------------

if (run_BPSO_parameter_selection) {
  
  cat(crayon::red("Start BPSO modelling \n"))
  
  # save all the results
  list_results <- list()
  
  total_runs <- nrow(base_tests_bpso)
  
  for (ifold in 1:nfolds) {
    cat(crayon::blue(sprintf("Start Fold: %d ---------------- \n", 
                             ifold)))
    
    trainDT_fold <- data[Fold != ifold]
    testDT_fold <- data[Fold == ifold]
    
    # create folder to save results 
    folder_name <- sprintf(base_name_folder, 
                           ifold)
    dir.create(folder_name, showWarnings = FALSE)
    
    for (i in 1:nrow(base_tests_bpso)) {
      cat(crayon::blue(sprintf("Fold: %d \t Run: %d/%d \n", 
                               ifold, 
                               i, 
                               total_runs)))
      
      # collect the parameters 
      pop_size <- base_tests_bpso[i,]$Popsize
      w <- base_tests_bpso[i,]$w
      k1 <- base_tests_bpso[i,]$k1
      k2 <- base_tests_bpso[i,]$k2
      transFun <- base_tests_bpso[i,]$TransFun[[1]]
      
      # run the BAOA method 
      result_run <- run_BPSO(train = trainDT_fold, 
                             test = testDT_fold, 
                             variables = variables, 
                             target_variable = target_variable, 
                             distribution_model = distribution_model, 
                             w = w, 
                             k1 = k1, 
                             k2 = k2, 
                             transferFun = transFun, 
                             pop_size = pop_size, 
                             max_stable = max_stable, 
                             max_iter = max_iter, 
                             order = order_interaction, 
                             offset = offset, 
                             concProb_type = concProb_type, 
                             location_glm_results = folder_name)
      
      # save the results 
      #number of iterations
      niter <- max(result_run[["AdditionalInfo"]][["Iteration"]])
      
      # put everything into a data.table
      result <- data.table("nIterations" = niter, 
                           "BestResult" = result_run[["ConcProbTestGLM"]], 
                           "ID" = i, 
                           "Fold" = ifold)
      list_results <- append(list_results, list(result))
      
      # in the data frame the position can be there multiple times --> unique
      # if multiple different coding have the same result --> take first one
      best_position <- unique(result_run[["AdditionalInfo"]][ConcProbTestGLM == result_run[["ConcProbTestGLM"]]][["Position"]])[1]
      
      # give some output 
      cat(green(sprintf("ID: %d - nIterations: %d - Result: %g - Position: %s \n", 
                        i, 
                        niter, 
                        result_run[["ConcProbTestGLM"]], 
                        best_position)))
    }
    
    # temporary store results
    saveRDS(rbindlist(list_results), 
            sprintf(here::here("Data", 
                              "Parameters", 
                              "Param_BPSO_DataSet%s_order%s_Fold%s.RDS"), 
                    data_set_number, 
                    order_interaction, 
                    ifold))
  }
  
  dt_results_cv_bpso <- rbindlist(list_results)
  
  dir.create(here::here("Data", 
                       "Parameters"), 
             showWarnings = FALSE)
  
  #Use the name of the Transfer function, rather than the object
  base_tests_temp <- copy(base_tests_bpso)
  base_tests_temp[, TransFunName := as.character(lapply(TransFun, \(x) x$get_name()))]
  base_tests_temp[, TransFun := NULL]
  
  # Link the results with the parameters selected
  dt_results_cv_bpso <- dt_results_cv_bpso |> 
    collapse::join(base_tests_temp, 
                   on = 'ID', 
                   how = "left")
  
  # store the results
  saveRDS(dt_results_cv_bpso, 
          sprintf(here::here("Data", 
                            "Parameters", 
                            "Param_BPSO_DataSet%s_order%s.RDS"), 
                  data_set_number, 
                  order_interaction))
}
