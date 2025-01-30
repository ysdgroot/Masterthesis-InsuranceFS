# Importing library -------------------------------------------------------

source(file.path("R", "Packages.R"))
source(file.path("R", "General Parameters.R"))

# Model run ---------------------------------------------------------------

## Running function --------------------------------------------------------
#TODO: problem with the GA, if the suggestions are bigger than the population size, it will cause an error

GA_sim_1 <- ga(fitness = mfitness, 
                  trainDT = trainDT, 
                  testDT = testDT, 
                  variableHandler = VH, 
                  targetVar = "claimNumber", 
                  distMod = poisson(link='log'), 
                  offset = "exposure", 
                  nullValue = 0.5,
                  withMain = TRUE,
                  location_save = full_folder_name,
             type = "binary", # optimization data type
             population = gabin_Population,
             suggestions = suggestions, 
             crossover = gabin_uCrossover,  # cross-over method
             selection = gabin_rwSelection, # roulette selection method
             mutation = gabin_raMutation, 
             elitism = 3, # best N indiv. to pass to next iteration
             pmutation = 0.2, # mutation rate prob
             popSize = 10, # number of the population in each evolution
             nBits = VH$get_length(), # total number of variables
             names = VH$get_used_variables(), # variable name
             run = 10, # max iter without improvement (stopping criteria)
             maxiter = 50, # total runs or generations
             monitor = plot, # plot the result at each iteration
             keepBest = TRUE, # keep the best solution at the end
             parallel = TRUE, # allow parallel processing
             seed=84211 # for reproducibility purposes
)


