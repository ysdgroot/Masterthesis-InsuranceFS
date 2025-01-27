# Importing library and Functions --------------------------------------------

source(file.path("R", "Packages.R"))
# source all the functions in the folder Functions
sapply(list.files(file.path("R", "Functions"), 
                  pattern = "*.R", 
                  full.names = TRUE), 
       FUN = "source", 
       echo = FALSE, 
       prompt.echo = FALSE)

# Load Data ---------------------------------------------------------------

inputDT <- readRDS(file.path("Data", "inputDT.rds"))

inputDT[, c('polNumb', 'claimNumbMD', 'claimSizeMD', 'claimSizeBI', 'age', 'density', 'carVal') := NULL]
setnames(inputDT, 'claimNumbBI', 'claimNumber')
setnames(inputDT, 'ageGrouped', 'age')
setnames(inputDT, 'carValGrouped', 'carVal')
setnames(inputDT, 'densityGrouped', 'density')

isFactorDT(inputDT, ,T)
isNumericDT(inputDT, ,T)

vars <- c('age', 'density', 'carVal', 'uwYear', 'gender', 
          'carType', 'carCat', 'job', 'group1', 'bm', 
          'nYears', 'cover')

# generate the training samples
trainSamp <- sample(c(1:nrow(inputDT)), 
                    round(0.8*nrow(inputDT)), 
                    replace = FALSE)

trainDT <- inputDT[trainSamp, ]
testDT <- inputDT[-trainSamp, ]

inputDTAll <- copy(inputDT)

# Variable Handler  -------------------------------------------------------
# this can convert the binary coding to the columns that needs to be selected

VH <- VariableHandler$new(variables = vars, 
                          order = 2)

# Fitness-function --------------------------------------------------------

#TODO: create function to transform and run the GLM model 
# reference for:
# H Uno, T Cai, M Pencina, R D'Agnostino and Lj Wei, 
# On the C-statistics for evaluating overall adequacy of risk prediction procedures with censored survival data, 
# Statistics in Medicine, 2011.
#



# TransferFunctions -------------------------------------------------------

baseClassTransferFunctions <- list(S1 = TransferFunction$new("S1", 
                                                             fun = function(x){sigmoid(x)}, 
                                                             type = "S"),
                                   S2 = TransferFunction$new("S2", 
                                                             fun = function(x){sigmoid(x/2)}, 
                                                             type = "S"), 
                                   S3 = TransferFunction$new("S3", 
                                                             fun = function(x){sigmoid(2*x)}, 
                                                             type = "S"), 
                                   V1 = TransferFunction$new("V1", 
                                                             fun = function(x){abs(x)/(sqrt(1 + x**2))}, 
                                                             type = "V"), 
                                   V2 = TransferFunction$new("V2", 
                                                             fun = function(x){abs(tanh(x))}, 
                                                             type = "V"), 
                                   V3 = TransferFunction$new("V3", 
                                                             fun = function(x){abs(2*atan(pi * x /2)/pi)}, 
                                                             type = "V"))

