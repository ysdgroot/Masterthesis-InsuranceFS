# Functions ---------------------------------------------------------------

add_folds <- function(dt, 
                      nfolds,
                      column, 
                      seed = 42) {
  set.seed(seed)
  
  dt[, Fold := createFolds(dt[[column]], 
                           k = nfolds, 
                           list = FALSE)]
  return(dt[])
}

save_data <- function(data, 
                      variables, 
                      target_variable, 
                      distribution_model, 
                      offset, 
                      concProb_type, 
                      data_set_number, 
                      folder = file.path("Data", "DataSets")){
  to_save <- list("Data" = data,
                  "Variables" = variables, 
                  "Target" = target_variable, 
                  "Offset" = offset, 
                  "ConcProbType" = concProb_type, 
                  "Distribution" = distribution_model) 
  saveRDS(to_save, 
          file = file.path(folder, 
                           sprintf("DataSet_%s.RDS", 
                                   data_set_number)))
}



# Setup -------------------------------------------------------------------

# Create a folder for the data sets
dir.create(file.path("Data", "DataSets"), 
           showWarnings = FALSE)

# Data Set 1 --------------------------------------------------------------

# same as pg15training but already grouped for age and density
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

inputDT <- add_folds(inputDT, 
                     nfolds = 5, 
                     column = "claimNumber")

# Data set number 1
save_data(inputDT, 
          variables = vars, 
          target_variable = target_variable, 
          distribution_model = poisson(), 
          offset = "exposure", 
          concProb_type = "bin", 
          data_set_number = 1)


# Data Set 2 --------------------------------------------------------------

# frequency
data("pg16trainpol")
setDT(pg16trainpol)
pg16trainpol

vars <- c("PolicyAgeCateg", 
          "CompanyCreation",
          "FleetMgt", 
          "Area", 
          "FleetSizeCateg", 
          "PayFreq", 
          "VehiclAge", 
          "Deduc", 
          "VehiclNb", 
          "PolicyCateg", 
          "VehiclCateg", 
          "BusinessType", 
          "ChannelDist", 
          "VehiclPower")
offset <- "Exposure"
target_variable <- "ClaimNb"
distribution_model <- poisson()

pg16trainpol <- add_folds(pg16trainpol, 
                     nfolds = 5, 
                     column = target_variable)

# Data set number 1
save_data(pg16trainpol, 
          variables = vars, 
          target_variable = target_variable, 
          distribution_model = poisson(), 
          offset = offset, 
          concProb_type = "bin", 
          data_set_number = 2)


# Data Set 3 --------------------------------------------------------------

# claims
data("pg16trainclaim")
setDT(pg16trainclaim)
pg16trainclaim

# joining the information to have the necessary information for the Severity
pg16trainclaim <- collapse::join(pg16trainclaim, 
                                 pg16trainpol, 
                                 on = c("PolicyID", 
                                        "LicNb", 
                                        "BeginDate"),
                                 multiple = TRUE)

pg16trainclaim[,  c('Year', 'BeginDate', 
                    'EndDate', 'Year_pg16trainpol', 
                    'EndDate_pg16trainpol', 'SumInsured', 
                    'ClaimNb', "VehiclNb") := NULL]

target_variable <- "ClaimCharge" 

# remove the negative amount, to be able to use the Gamma
pg16trainclaim <- pg16trainclaim[ClaimCharge > 0]

pg16trainclaim <- add_folds(pg16trainclaim, 
                          nfolds = 5, 
                          column = target_variable)

# data <- 
vars <- c('PolicyAgeCateg', 
          "CompanyCreation", 
          "FleetMgt", 
          "Area", 
          "FleetSizeCateg", 
          "PayFreq", 
          "VehiclAge", 
          "Deduc", 
          "PolicyCateg", 
          "VehiclCateg", 
          "BusinessType", 
          "ChannelDist", 
          "VehiclPower")
distribution_model <- Gamma(link = "log")
target_variable <- "ClaimCharge"
offset <- NULL

# Data set number 1
save_data(pg16trainclaim, 
          variables = vars, 
          target_variable = target_variable, 
          distribution_model = distribution_model, 
          offset = offset, 
          concProb_type = "cont", 
          data_set_number = 3)

# # Data Set 4 --------------------------------------------------------------
# 
# # Data set number 4 & 5
# # claims and frequency 
# data('beMTPL97')
# setDT(beMTPL97)
# beMTPL97
# 
# ## Frequency
# 
# # data <- 
# vars <- c()
# offset <- "expo"
# target_variable <- "nclaims"
# # all except postcode, long, lat 
# #TODO: category agec, ageph, and power? 
# distribution_model <- poisson()
# 
# 
# # Data Set 5 --------------------------------------------------------------
# 
# ## Severity 
# vars <- c()
# offset <- NULL 
# target_variable <- "amount"
# distribution_model <- Gamma(link = "log")
