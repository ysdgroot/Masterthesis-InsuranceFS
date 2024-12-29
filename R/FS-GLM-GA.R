# using the GA library to create the Feature selection method for the GLM model 

# website where the Feature Selection is performed 
# https://towardsdatascience.com/feature-selection-using-genetic-algorithms-in-r-3d9252f1aa66

# some example of the GA algorithm
# https://cran.r-project.org/web/packages/GA/vignettes/GA.html

# Importing library -------------------------------------------------------

library(GA)
source(file.path("R", "Packages.R"))
source(file.path("R", "General Parameters.R"))

# source all the functions for the GA
sapply(list.files(file.path("R", "GA"), 
                  pattern = "*.R", 
                  full.names = TRUE), 
       FUN = "source", 
       echo = FALSE, 
       prompt.echo = FALSE)


# Model run ---------------------------------------------------------------

## Parameters --------------------------------------------------------------

## Running function --------------------------------------------------------

mfitness <- memoise::memoise(concProb_glm_bin)

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

# Run ---------------------------------------------------------------------

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
             nBits = VH$getLength(), # total number of variables
             names= VH$getUsedVariables(), # variable name
             run=10, # max iter without improvement (stopping criteria)
             maxiter = 50, # total runs or generations
             monitor=plot, # plot the result at each iteration
             keepBest = TRUE, # keep the best solution at the end
             parallel = TRUE, # allow parallel processing
             seed=84211 # for reproducibility purposes
)


