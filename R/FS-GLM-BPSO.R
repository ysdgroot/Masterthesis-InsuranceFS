# Importing library -------------------------------------------------------

source(file.path("R", "Packages.R"))
source(file.path("R", "General Parameters.R"))

# source all the functions for the GA
sapply(list.files(file.path("R", "BPSO"), 
                  pattern = "*.R", 
                  full.names = TRUE), 
       FUN = "source", 
       echo = FALSE, 
       prompt.echo = FALSE)


# Model run ---------------------------------------------------------------

## Parameters --------------------------------------------------------------

mfitness <- memoise::memoise(concProb_glm_bin)

set.seed(8)

# random suggestion sample 
suggestions <- matrix(as.double(NA), 
                      nrow = 10, 
                      ncol = VH$getLength())
for(j in 1:10) { positions <- sample.int(n = VH$getLength(), 
                                         size = 20)
suggestion <- rep(0,  VH$getLength())
suggestion[positions] <- 1

suggestions[j,] <- suggestion
}

suggestions <- rbind(VH$getCoding(vars), 
                     suggestions)

## Running function --------------------------------------------------------

BinarySwarm$debug("runProcess")

BPSO_sim1 <- BinarySwarm$new(5, 
                             nBits = VH$getLength(), 
                             w = 1, 
                             k1 = 2,
                             k2 = 3,
                             transferFun = baseClassTransferFunctions$S1, 
                             suggestions = NULL, 
                             chanceBit = 0.2, 
                             seed = 739)

BPSO_sim1$runProcess(fun = concProb_glm_bin,
                     maxIter = 1,
                     argsFun = list(trainDT = trainDT, 
                                    testDT = testDT, 
                                    distMod = poisson(link = "log"), 
                                    variableHandler = VH, 
                                    offset = "exposure", 
                                    targetVar = "claimNumber", 
                                    withMain = TRUE, 
                                    message = FALSE))
