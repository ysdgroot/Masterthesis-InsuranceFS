#before: nMinInOptModsGen <- rep(1, nGens);nAddedBestModsGen = rep(5, nGens), nModsGen <- rep(10, nGens)
#test1: nMinInOptModsGen <- c(1, 1, 1, rep(3, nGens - 3));nAddedBestModsGen = rep(2, nGens) #making sure that the algo does not converge too soon, nModsGen <- rep(10, nGens)
#test2: nMinInOptModsGen <- c(1, 1, 1, rep(3, nGens - 3));nAddedBestModsGen = rep(3, nGens) #making sure that the algo does not converge too soon, nModsGen <- rep(10, nGens)
#test3: nMinInOptModsGen <- c(1, 1, 1, rep(3, nGens - 3));nAddedBestModsGen = c(rep(3, 5), rep(5, nGens - 5)),  #making sure that the algo does not converge too soon, nModsGen <- rep(15, nGens)
#test4: nMinInOptModsGen <- c(1, 1, 1, rep(3, nGens - 3));nAddedBestModsGen = c(rep(3, 5), rep(5, nGens - 5)),  #making sure that the algo does not converge too soon, nModsGen <- rep(30, nGens)
#test5: nMinInOptModsGen <- rep(1, nGens);nAddedBestModsGen = c(rep(3, 5), rep(5, nGens - 5)),  #making sure that the algo does not converge too soon, nModsGen <- rep(30, nGens)

#adapting nMinInOptModsGen made the algo fit much faster since we reduce the model complexity over the generations (some covars are noise only, and they are just thrown out if not useful)
#nAddedBestModsGen needs to be high enoug (was not the case for test1) to make sure that the algo converges
#nModsGen needs to be high enough to make sure that there is enough randomness in the population (but not too big -> elsewise it will take too long to fit)

rm(list = ls())


# Loading packages and functions ------------------------------------------

source(file.path("R","Packages.R"))

# source all the functions in the folder Functions
sapply(list.files(file.path("R", "Functions"), 
                  pattern = "*.R", 
                  full.names = TRUE), 
       FUN = "source", 
       echo = FALSE, 
       prompt.echo = FALSE)
source(file.path("R", "iterGA.R"))
#TODO: replace the function "allFirstOrderIntTerms" with "allOrderIntTerms"

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


# Initialization GA -------------------------------------------------------

seedNumb <- 713
set.seed(seedNumb)

nGens <- 4 # number of generations 
nModsGen <- rep(30, nGens) # number of models for each generation
nCrossOverGen <- rep(1, nGens) # number of cross-overs
nMutsGen <- rep(1, nGens) # number of mutations
nVarInit <- 10 # number of initial variables
#TODO: make this nVarMax a list to decrease the number of variables used at the end
nVarMax <- 21 # maximum number of variables 
nRedMods <- 5 # number of repeated modeling when there are to much variables selected
trainPerc <- 0.5 # percentage for the training set 
testPerc <- NULL
valOffSet <- 'exposure' # offset column name
#valOffSet <- NULL
# nMinInOptModsGen <- c(0,0,0,0,0,0,0,1,0,0)
# nMinInOptModsGen <- NULL
nMinInOptModsGen <- rep(1, nGens) # minimal number of times variable should appear in final model -- There are more details below
includeIntsGen <- rep(FALSE, nGens) # if interactions should be taken into account
# nModsIntGen <- rep(c(0,2), nGens/2)
nModsIntGen <- rep(0, nGens) # number of models with interactions modelled
# nMaxIntGen <- rep(c(0,5), nGens/2)
nMaxIntGen <- rep(0, nGens) # maximum number of interactions added to the model

##nAddedBestModsGen <- c(rep(3, 5), rep(5, nGens - 5)) # selection of the best previous models
nAddedBestModsGen <- c(rep(3, 5), rep(5, 2))


#added 
ratioConcProbMSE <- 0 #1 = MSE, 0 = concProb
ilst2Save <- NULL
distMod <- poisson(link='log')

statusList <- NULL
upperBoundPred <- NULL

#TODO: call it "ignoreVariables"
# badOnes <- c('privateUse*coverType', 'coverType*privateUse', 'age*experience', 'experience*age')
badOnes <- c()
saveStatus <- TRUE
partialSaveName <- NULL


# Run the GA function -----------------------------------------------------



GA_results <- functionGA(inputDT = trainDT, 
                         varsGA = varsGA, 
                         nModsGen = nModsGen, 
                         nGens = nGens, 
                         nCrossOverGen = nCrossOverGen, 
                         nMutsGen = nMutsGen,
                         nVarInit = nVarInit, 
                         nVarMax = nVarMax, 
                         nRedMods = 5, 
                         trainPerc = 0.8, 
                         testPerc = NULL,
                         distMod = poisson(link='log'), 
                         nAddedBestModsGen = rep(5, nGens), 
                         valOffSet = 'exposure',
                         nMinInOptModsGen = NULL, 
                         includeIntsGen = NULL, 
                         nModsIntGen = NULL, 
                         nMaxIntGen = NULL, 
                         saveStatus = TRUE, 
                         partialSaveName = 'statusList_Gen', 
                         statusList = NULL,
                         typeThreshold = NULL,
                         ratioConcProbMSE = NULL,
                         upperBoundPred = NULL, 
                         badOnes = NULL, 
                         observed =  "claimNumber")

save.image(file.path("Data", "test_all.RData"))



