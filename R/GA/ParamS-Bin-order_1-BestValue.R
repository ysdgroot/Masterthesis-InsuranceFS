# Best parameters ---------------------------------------------------------

dt_results_cv_ga <- readRDS(file.path("Data", 
                                        "Parameters", 
                                        "Param_GA_bin.RDS"))

base_tests <- unique(dt_results_cv_ga[, .SD, 
                                        .SDcols = !c("nIterations", 
                                                     "BestResult", 
                                                     "Fold")])

avg_results <- dt_results_cv_ga[, .(AvgResult = mean(BestResult),
                                      AvgIteration = mean(nIterations)),
                   by = c("ID")][order(-AvgResult, AvgIteration)] 


avg_results <- avg_results |> 
  collapse::join(base_tests, 
                 on = "ID", 
                 how = "left")

avg_results[AvgResult == max(AvgResult)]

# this is some manual selection, because the results are close

# best selection 
best_results_GA <- data.table(n_elits = 4, 
                              pop_size = 25, 
                              p_crossover = 0.6, 
                              p_mutation = 0.4, 
                              name_selection = "linear")

save_location <- file.path("Data", 
                           "Parameters", 
                           "Best_GA_order_1.RDS")

saveRDS(best_results_GA, 
        save_location)


