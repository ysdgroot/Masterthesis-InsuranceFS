# Importing library -------------------------------------------------------

source(file.path("R", "0-Packages.R"))
source(file.path("R", "1-General Parameters.R"))
# load the config file of order one to have the same folds to work with
source(file.path("R", "1-Config-order1.R")) 


# Coding GBM --------------------------------------------------------------

library(xgboost)
library(Matrix)

# hyperparameters selection 
# gbtree or gblinear
#

fold_number <- 1
targetVar <- "claimNumber"

# put back into a numeric value
df <- inputDT[Fold != fold_number, 
              .SD,
              .SDcols = -c("Fold")]

cols <- colnames(df)
df[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]



#watchlist = list(train=xgb_train, test=xgb_test)

sparse_matrix <- sparse.model.matrix(claimNumber ~ uwYear +
                                       gender +
                                       carType+
                                       job +
                                       group1 +
                                       bm +
                                       nYears +
                                       cover +
                                       age +
                                       carVal +
                                       density +
                                       gender:uwYear, 
                                     data = df, 
                                     base_margin)[,-1]

# test dataset 
df_test <- inputDT[Fold == fold_number, 
              .SD,
              .SDcols = -c("Fold")]

cols <- colnames(df_test)
df_test[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
sparse_matrix_test <- sparse.model.matrix(claimNumber ~ uwYear +
                                       gender +
                                       carType+
                                       job +
                                       group1 +
                                       bm +
                                       nYears +
                                       cover +
                                       age +
                                       carVal +
                                       density +
                                       gender:uwYear, 
                                     data = df_test)[,-1]


labels <-inputDT[Fold != fold_number,.SD, .SDcols = targetVar] |> unlist()

# 

# add the C-index to the results 
test_xg <- xgboost(data = sparse_matrix, 
                    label = labels,
                    nthread = 4, 
                    objective = "count:poisson", 
                    nrounds = 50, 
                   booster = "gbtree")

var_importance_xgb <- xgb.importance(colnames(sparse_matrix), model = test_xg)
var_importance_xgb[, CumGain := cumsum(Gain)][CumGain <= 0.95][,1]

variables_used <- var_importance_xgb[, CumGain := cumsum(Gain)][CumGain <= 0.95][,1]

concProb_glm_fastglm(coding = VH$get_coding(variables_used, 
                                            withMain = TRUE), 
                     trainDT = trainDT, 
                     testDT = testDT, 
                     distMod = poisson(), 
                     variableHandler = VH, 
                     targetVar = targetVar, 
                     nullValue = null_value, 
                     withMain = TRUE)


# Calculate the Concordance probability -----------------------------------

(result_traindata <- concProb_bin_fast(labels, 
                                       predict(test_xg, 
                                               sparse_matrix))$concProb)


predict(test_xg, 
        newdata = sparse_matrix)




