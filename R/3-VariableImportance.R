# Variable importance -----------------------------------------------------

# Importing library -------------------------------------------------------

source(file.path("R", "0-Packages.R"))

# -------------------------------------------------------------------------
# GA ----------------------------------------------------------------------

results <- BPSO_run$AllResults

# -------------------------------------------------------------------------
variable_importance(results, 
                    column_name = "TestData", 
                    var_names = VH$get_used_variables()) |> sort(decreasing = TRUE)

variable_importance(results, 
                    column_name = "AIC", 
                    var_names = VH$get_used_variables()) |> sort(decreasing = FALSE)

variable_importance(results, 
                    column_name = "BIC", 
                    var_names = VH$get_used_variables()) |> sort(decreasing = FALSE)


# BPSO --------------------------------------------------------------------





# BAOA --------------------------------------------------------------------




# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# XGBOOST -----------------------------------------------------------------




