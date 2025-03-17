# Variable ----------------------------------------------------------------

null_value <- 0.5

# Variable Handler  -------------------------------------------------------
# this can convert the binary coding to the columns that needs to be selected

order <- 1
VH <- VariableHandler$new(variables = vars, 
                          order = order)

# Creation of Folds -------------------------------------------------------
# adding folds for cases when CV is used, if no CV then only the First fold is used

# reproducible random sampling
seed <- 42
set.seed(seed)

nfolds <- 5

inputDTAll <- copy(inputDT)
inputDT[, Fold := createFolds(inputDT$claimNumber, 
                              k = nfolds, 
                              list = FALSE)]

# to speed up data selection based on the folds
setkeyv(inputDT, "Fold")

# selection of the base train and test sets
trainDT <- inputDT[Fold != 1]
testDT <- inputDT[Fold == 1]


# creation of the folders to store the information 
base_name_folder <- "Seed%d_Fold%d-%d_%s_n%d_null%g"

(folder_name <- sprintf(base_name_folder, 
                        seed,
                        1, 
                        nfolds, 
                        "bin", 
                        VH$get_length(), 
                        null_value))
full_folder_name <- file.path("Data", folder_name)

dir.create(full_folder_name, showWarnings = FALSE)

# Fitness-function --------------------------------------------------------

mfitness <- memoise::memoise(concProb_glm_fastglm)
