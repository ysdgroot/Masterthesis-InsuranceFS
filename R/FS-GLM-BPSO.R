# Importing library -------------------------------------------------------

source(file.path("R", "Packages.R"))

source(file.path("R", "General Parameters.R"))

# Model run ---------------------------------------------------------------

## Running function --------------------------------------------------------

BPSO_gen <- BPG_Velocity$new(ParticleBPSO)
BPSO_swarm <- SwarmBPSO$new(10, 
                            VH$get_length(), 
                            transferFun = baseClassTransferFunctions$S1, 
                            BPSO_gen, 
                            w = 1, 
                            k1 = 2, 
                            k2 = 3, 
                            chanceBit = 0.2, 
                            seed = 739, 
                            suggestions = NULL)

BPSO_run <- BPSO_swarm$run_process(concProb_glm_fastglm, 
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

test_res_BPSO <- result_2_dt(BPSO_run$AllResults)

