install.packages(c("data.table", 
                   "R2DT", 
                   "plyr", 
                   "devFunc", 
                   "mgcv", 
                   "speedglm", 
                   "fastglm", 
                   "caret", 
                   "collapse", 
                   "crayon", 
                   "memoise", 
                   "sigmoid", 
                   "Matrix", 
                   "parallel", 
                   "doParallel"))
install.packages("xgboost")
install.packages("glmnet")
install.packages("purrr")
install.packages("matrixStats")
install.packages("glmnet")

install.packages("devtools")
devtools::install_github("dutangc/CASdatasets")
devtools::install_github("JolienPonnet/fastConcProb")

# package of GA
devtools::install_github("ysdgroot/GA")
# own package
devtools::install_github("ysdgroot/MHFS")
