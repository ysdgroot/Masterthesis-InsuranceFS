# Best parameters ---------------------------------------------------------

dt_results_cv_bpso <- readRDS(file.path("Data", 
                                        "Parameters", 
                                        "Param_BPSO_bin.RDS"))

base_tests <- unique(dt_results_cv_bpso[, .SD, 
                                        .SDcols = !c("nIterations", 
                                                     "BestResult")])

avg_results <- dt_results_cv_bpso[, .(AvgResult = mean(BestResult),
                                      AvgIteration = mean(nIterations)),
                   by = c("ID")][order(-AvgResult, AvgIteration)] 


avg_results <- avg_results |> 
  collapse::join(base_tests, 
                 on = "ID", 
                 how = "left")

avg_results[AvgResult == max(AvgResult)]

# this is some manual selection, because the results are close

# best selection 
best_results_BPSO <- data.table(k1 = 5, 
                                k2 = 2.5, 
                                w = 1.1, 
                                TransFunName = "V2", 
                                Popsize = 20)

save_location <- file.path("Data", 
                           "Parameters", 
                           "Best_BPSO_order_1.RDS")

saveRDS(best_results_BPSO, 
        save_location)


