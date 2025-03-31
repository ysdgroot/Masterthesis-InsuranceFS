
# Lasso -------------------------------------------------------------------

# Importing library -------------------------------------------------------

source(file.path("R", "0-Packages.R"))

library(glmnet)
library(parallel)

# Data import -------------------------------------------------------------

source(file.path("R", "1-General Parameters.R"))
source(file.path("R", "1-Config-order1.R"))

# Settings ----------------------------------------------------------------

# parameters for the lasso 
alpha <- 1 # set Elastic Net to a Lasso penalty

fold_number <- 1
targetVar <- "claimNumber"

# -------------------------------------------------------------------------

# put back into a numeric value
df <- inputDT[Fold != fold_number,.SD,
              .SDcols = -c("Fold")]

# convert for the SparseMatrix; only to need the importance of the variable and/or interaction 
# if all the betas would be needed, the transformation is not needed
cols <- colnames(df)
df[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]

y <- df$claimNumber
offset_model <- df$exposure

# create sparse matrix for the construction input for the LASSO regression
sparse_matrix <- sparse.model.matrix(VH$get_formula(coding = rep(1, VH$get_length()), 
                                                    distMod = poisson(), 
                                                    targetVar = targetVar), 
                                     data = df)[,-1]
model_cv <- cv.glmnet( sparse_matrix, 
                       y, 
                       family = "poisson", 
                       offset = offset_model, 
                       parallel = TRUE, 
                       nfolds = 5, 
                       alpha = alpha)
lambda_min <- model_cv$lambda.min

plot(model_cv)

lasso_model <- glmnet(x = sparse_matrix, 
                      y, 
                      family = "poisson", 
                      offset = offset_model, 
                      lambda = NULL, 
                      alpha = alpha)

coef(lasso_model,
     s = lambda_min)


all_variabless <- VH$get_all_variables()

coef_model <- coef(lasso_model,
                   s = lambda_min)
  
variables_selected <- setdiff(all_variabless, 
                              rownames(coef_model)[which(coef_model == 0)])

test <- coef(lasso_model,
             s = lambda_min)

# plot the coefficients based on the lambda
plot(lasso_model, 
     label = TRUE)


# -------------------------------------------------------------------------

# prediction 
#predict(fit, newx = x[1:5,], type = "response", s = 0.05)

predict(lasso_model, 
        s = lambda_min, 
        type = "response", 
        newx = sparse_matrix, 
        newoffset =offset_model)


df_test <- inputDT[Fold == fold_number,
                  .SD,
                  .SDcols = -c("Fold")]


df_test[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]



(result_traindata <- concProb_bin_fast(df[[targetVar]], 
                                     predict(lasso_model, 
                                             s = lambda_min, 
                                             type = "response", 
                                             newx = sparse_matrix, 
                                             newoffset =offset_model))$concProb)

offset_model_test <- df_test$exposure
(result_testdata <- concProb_bin_fast(df_test[[targetVar]], 
                                      predict(lasso_model, 
                                              s = lambda_min, 
                                              type = "response", 
                                              newx = sparse.model.matrix(VH$get_formula(coding = rep(1, VH$get_length()), 
                                                                                        distMod = poisson(), 
                                                                                        targetVar = targetVar), 
                                                                         data = df_test)[,-1], 
                                              newoffset =offset_model_test))$concProb)



#TODO: for modelling with LASSO and XGBOOST
# STEP 1: create "best" model 
# STEP 2: Do prediction on the Test data set (Fold = 1) and calculate Concordance probability
# STEP 3: Get the variables used based on the model 
# STEP 4: Refit the GLM model based on the variables selected 
# STEP 5: Calculate the Concordance Probability
# STEP 6: Compare the results


# Step 4: Refit the GLM model with the selected variables -----------------
# and calculate the Concordance probability
concProb_glm_fastglm(coding = VH$get_coding(variables_selected, 
                                            withMain = TRUE), 
                     trainDT = trainDT, 
                     testDT = testDT, 
                     distMod = poisson(), 
                     variableHandler = VH, 
                     targetVar = targetVar, 
                     nullValue = null_value, 
                     withMain = TRUE)

# LASSO model gives a better results compared to the GLM model with the selected variables





