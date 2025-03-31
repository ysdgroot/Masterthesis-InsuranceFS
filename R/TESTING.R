text <- '-'
for(i in 1:10){
  text <- paste0(text, "-")
  cat(sprintf('\r %s', text))
}

position <- c(0, 0, 1, 0, 0, 0,
              0, 0, 0, 0, 0, 0)

# problem with V1 - BAOA -
list_transfun[[2]]$changePosition(position, 
                                  position)


# -------------------------------------------------------------------------

# function to calculate the minimum values based on the MH 
# With inputs: 
#     dataset
#     parameters of MH
#
#
# this way the stability can be more easily tested 


minimize_bpso <- function(dataset, 
                          fun, 
                          VH,
                          k1, 
                          k2, 
                          w, 
                          pop_size, 
                          transFun, 
                          args_fun = list(),
                          max_iter = 100, 
                          max_stable = 10,
                          chance_bit = 0.2, 
                          seed = 420){
  
  # creation of a particle generator for the BPSO
  BPSO_gen <- BPG_Velocity$new(ParticleBPSO, 
                               chance_bit = chance_bit,
                               suggestions = NULL)
  
  # create Swarm
  BPSO_swarm <- SwarmBPSO$new(pop_size, 
                              VH$get_length(), 
                              transferFun = transFun, 
                              BPSO_gen, 
                              w = w, 
                              k1 = k1, 
                              k2 = k2, 
                              seed = seed)
  # run the BPSO process
  BPSO_run <- BPSO_swarm$run_process(concProb_glm_fastglm, 
                                     max_stable = max_stable, 
                                     max_iter = max_iter,
                                     args_fun = args_fun, 
                                     seed = seed)
  return(BPSO_run)
}


minimize_baoa <- function(dataset, 
                          fun, 
                          VH, 
                          beta, 
                          k, 
                          minMoa, 
                          maxMoa, 
                          pop_size, 
                          transFun,
                          args_fun = list(),
                          max_iter = 100, 
                          max_stable = 10, 
                          seed = 420){
  
  # creation of a particle generator for the BAOA
  BAOA_gen <- BPG$new(ParticleBAOA,
                      chance_bit = 0.2,
                      suggestions = NULL)
  
  # create swarm
  BAOA_swarm <- SwarmBAOA$new(pop_size, 
                              VH$get_length(), 
                              transferFun = transFun, 
                              BAOA_gen, 
                              beta = beta, 
                              k = k, 
                              minMoa = minMoa, 
                              maxMoa = $maxMoa, 
                              seed = seed)
  
  # run the process
  BAOA_run <- BAOA_swarm$run_process(fun, 
                                     max_stable = max_stable, 
                                     max_iter = max_iter,
                                     args_fun = args_fun, 
                                     seed = seed)
  
  return(BAOA_run)
}

minimize_ga <- function(dataset, 
                        fun, 
                        VH, 
                        p_crossover, 
                        p_mutation, 
                        selection, 
                        n_elits, 
                        pop_size, 
                        ..., 
                        max_iter = 100,
                        max_stable = 10, 
                        parallel = FALSE, 
                        seed = 420){
  
  
  GA_sim <- ga(fitness = fun, 
               ...,
               type = "binary", # optimization data type
               population = gabin_Population,
               suggestions = NULL,
               selection = selection, 
               crossover = gabin_uCrossover,  # cross-over method
               pcrossover = p_crossover,
               mutation = gabin_raMutation, # uniform random 
               pmutation = p_mutation, 
               elitism = n_elits, # best N indiv. to pass to next iteration
               popSize = pop_size, # number of the population in each evolution
               nBits = VH$get_length(), # total number of variables
               names = VH$get_used_variables(), # variable name
               run = max_stable, # max iter without improvement (stopping criteria)
               maxiter = max_iter, # total runs or generations
               monitor = FALSE, 
               keepBest = TRUE, # keep the best solution at the end
               parallel = parallel, # don't do parallel because it takes to much time 
               seed=seed)
  
  
  # put all the results into a data.table
  all_results <- convert_resultGA(GA_sim@allResults)
  
  # best results and position 
  best_result <- list(Position = GA_sim@solution, 
                      Result = GA_sim@fitnessValue)
  
  list_return <- list(AllResults = all_results, 
                      BestResult = best_result)
  
  return(list_return)
}




