# using the GA library to create the Feature selection method for the GLM model 

# website where the Feature Selection is performed 
# https://towardsdatascience.com/feature-selection-using-genetic-algorithms-in-r-3d9252f1aa66

# some example of the GA algorithm
# https://cran.r-project.org/web/packages/GA/vignettes/GA.html

#TODO: don't forget to reference the package 
# Importing library -------------------------------------------------------

library(GA)
source(file.path("R", "Packages.R"))
# source all the functions in the folder Functions
sapply(list.files(file.path("R", "Functions"), 
                  pattern = "*.R", 
                  full.names = TRUE), 
       FUN = "source", 
       echo = FALSE, 
       prompt.echo = FALSE)
source(file.path("R", "iterGA.R"))

# Load Data ---------------------------------------------------------------

inputDT <- readRDS(file.path("Data", "inputDT.rds"))

inputDT[, c('polNumb', 'claimNumbMD', 'claimSizeMD', 'claimSizeBI', 'age', 'density', 'carVal') := NULL]
setnames(inputDT, 'claimNumbBI', 'claimNumber')
setnames(inputDT, 'ageGrouped', 'age')
setnames(inputDT, 'carValGrouped', 'carVal')
setnames(inputDT, 'densityGrouped', 'density')

# claimNumbBI ~ s(age) + s(density) + s(carVal) + uwYear + gender + carType + carCat + job + group1 + bm + nYears + cover + offset(exposure)

isFactorDT(inputDT, ,T)
isNumericDT(inputDT, ,T)

vars <- c('age', 'density', 'carVal', 'uwYear', 'gender', 'carType', 'carCat', 'job', 'group1', 'bm', 'nYears', 'cover')
varsGA <- c(vars, allFirstOrderIntTerms(vars))

trainSamp <- sample(c(1:nrow(inputDT)), round(0.8*nrow(inputDT)), replace = FALSE)
testSamp <- createTestSet(inputDT[,.SD,.SDcol = unique(vars)], trainSamp, (nrow(inputDT) - length(trainSamp)))

trainDT <- inputDT[trainSamp, ]
testDT <- inputDT[testSamp, ]

inputDTAll <- copy(inputDT)
inputDT <- copy(trainDT)

VH <- variableHandler$new(variables = vars, 
                          order = 2)

# Construction ------------------------------------------------------------

#TODO: create function to transform and run the GLM model 
#TODO: check the concordance function of 'Survival' package and the one from Jolien
# reference for:
# H Uno, T Cai, M Pencina, R D'Agnostino and Lj Wei, 
# On the C-statistics for evaluating overall adequacy of risk prediction procedures with censored survival data, 
# Statistics in Medicine, 2011.
#


# it could be a bootstrap function
#TODO: to collect the information for the training dataset, 
#     save the results with given location 
concProb_glm_bin <- function(coding, 
                             trainDT, 
                             testDT, 
                             distMod, 
                             variableHandler,
                             nullValue = 0.5,
                             maxVar = 20, 
                             offset = "exposure", 
                             targetVar = "ClaimNumber", 
                             withMain = TRUE){
  
  used_variables <- variableHandler$getVariables(coding = coding, 
                                                 withMain = withMain)
  
  # set penalty if there are more than desired number of features 
  if (length(used_variables) > maxVar){
    return(1e-5)
  }
  
  formula_glm <- getForm_v2(distMod = distMod, 
                             targetVar = targetVar, 
                             varList = used_variables, 
                             offset = offset)
  
  fitModel <- bam(formula = formula_glm, 
                  family = distMod, 
                  data = trainDT, 
                  chunk.size = min(10000, nrow(trainDT)))
  
  predModel <- predict(fitModel, 
                       testDT, 
                       type = 'response')
  
  result <- concProb_bin_fast(testDT[[targetVar]], predModel)$concProb
  # remove the NullValue such that the selection is better 
  result <- result - nullValue
  
  return(result)
}


# Model run ---------------------------------------------------------------
#TODO: check the kofnGA package, to put a constraint on the total number of features selected

#TODO: make function for "PostFitness"


## Parameters --------------------------------------------------------------

## Running function --------------------------------------------------------
library(memoise)
library(parallel)
library(doParallel)

mfitness <- memoise(concProb_glm_bin)

# random suggestion sample 
suggestions <- matrix(as.double(NA), 
                      nrow = 10, 
                      ncol = VH$length)
for(j in 1:10) { positions <- sample.int(n = VH$length, 
                                         size = 20)
suggestion <- rep(0,  VH$length)
suggestion[positions] <- 1

suggestions[j,] <- suggestion
}

suggestions <- rbind(VH$getCoding(vars), 
                     suggestions)

#TODO: detectCores, so that it is not using my computer to much
ga_GA_1 <- ga(fitness = concProb_glm_bin, 
             trainDT = trainDT, 
             testDT = testDT, 
             distMod = poisson(link='log'), 
             offset = "exposure", 
             targetVar = "claimNumber", 
             variableHandler = VH, 
             nullValue = 0.5,
             maxVar = 30, 
             withMain = TRUE,
             type = "binary", # optimization data type
             population = gabin_Population,
             suggestions = VH$getCoding(vars), # 
             crossover = gabin_uCrossover,  # cross-over method
             selection = gabin_rwSelection, # roulette selection method
             mutation = gabin_raMutation, 
             elitism = 3, # best N indiv. to pass to next iteration
             pmutation = 0.2, # mutation rate prob
             popSize = 10, # number of the population in each evolution
             nBits = VH$length, # total number of variables
             names= VH$all_variables, # variable name
             run=10, # max iter without improvement (stopping criteria)
             maxiter = 50, # total runs or generations
             monitor=plot, # plot the result at each iteration
             keepBest = TRUE, # keep the best solution at the end
             parallel = FALSE, # allow parallel processing
             seed=84211 # for reproducibility purposes
)

# version of the new GA package
# ga_GA_1 <- ga(fitness = concProb_glm_bin, 
#               trainDT = trainDT, 
#               testDT = testDT, 
#               distMod = poisson(link='log'), 
#               offset = "exposure", 
#               targetVar = "claimNumber", 
#               variableHandler = VH, 
#               nullValue = 0.5,
#               maxVar = 30, 
#               withMain = TRUE,
#               maxBitsSelection = bitSelection,
#               callArgsMaxBits = list(maxBits = 20, 
#                                      size = 5),
#               type = "binary", # optimization data type
#               population = gabin_Population,
#               suggestions = suggestions, # 
#               crossover = gabin_uCrossover,  # cross-over method
#               selection = gabin_rwSelection, # roulette selection method
#               mutation = gabin_raMutation, 
#               elitism = 3, # best N indiv. to pass to next iteration
#               pmutation = 0.2, # mutation rate prob
#               popSize = 10, # number of the population in each evolution
#               nBits = VH$length, # total number of variables
#               names= VH$all_variables, # variable name
#               run=2, # max iter without improvement (stopping criteria)
#               maxiter = 4, # total runs or generations
#               monitor=plot, # plot the result at each iteration
#               keepBest = TRUE, # keep the best solution at the end
#               parallel = TRUE, # allow parallel processing
#               seed=84211 # for reproducibility purposes
# )

