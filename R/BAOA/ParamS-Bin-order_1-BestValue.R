# Best parameters ---------------------------------------------------------

dt_results_cv_baoa <- readRDS(file.path("Data", 
                                        "Parameters", 
                                        "Param_BAOA_bin.RDS"))

base_tests <- unique(dt_results_cv_baoa[, .SD, 
                                        .SDcols = !c("nIterations", 
                                                     "BestResult", 
                                                     "Fold")])

avg_results <- dt_results_cv_baoa[, .(AvgResult = mean(BestResult),
                                      AvgIteration = mean(nIterations)),
                                  by = c("ID")][order(-AvgResult, AvgIteration)] 


avg_results <- avg_results |> 
  collapse::join(base_tests, 
                 on = "ID", 
                 how = "left")

avg_results[AvgResult == max(AvgResult)]

# this is some manual selection, because the results are close

# best selection 
best_results_BAOA <- data.table(beta = 5, 
                                k = 0.7, 
                                minMoa = 0.1, 
                                maxMoa = 0.6, 
                                TransFunName = "V1", 
                                Popsize = 25)

save_location <- file.path("Data", 
                           "Parameters", 
                           "Best_BAOA_order_1.RDS")

saveRDS(best_results_BAOA, 
        save_location)
