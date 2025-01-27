# Importing library -------------------------------------------------------

source(file.path("R", "Packages.R"))

source(file.path("R", "General Parameters.R"))

# Model run ---------------------------------------------------------------

## Running function --------------------------------------------------------

BAOA_gen <- BPG$new(ParticleBAOA)
BAOA_swarm <- SwarmBAOA$new(10, 
                            VH$get_length(), 
                            transferFun = baseClassTransferFunctions$S1, 
                            BAOA_gen, 
                            beta = 5, 
                            k = 5, 
                            minMoa = 0.1, 
                            maxMoa = 1, 
                            chanceBit = 0.2, 
                            seed = 739, 
                            suggestions = NULL)

BAOA_run <- BAOA_swarm$run_process(concProb_glm_fastglm, 
                                   max_stable = 5, 
                                   max_iter = 10,
                                   args_fun = list(
                                     trainDT = trainDT, 
                                     testDT = testDT, 
                                     distMod = poisson(link = "log"), 
                                     variableHandler = VH, 
                                     nullValue = 0.5, 
                                     type = "bin", 
                                     offset = "exposure", 
                                     targetVar = "claimNumber", 
                                     location_save = full_folder_name
                                   ), 
                                   seed = 123)

test_res_BPSO <- result_2_dt(BAOA_run$AllResults)