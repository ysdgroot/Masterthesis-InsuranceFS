#' @title BinarySwarm
#' 
#' @description
#' Abstract base class for a Binary Swarm
#' 
R6::R6Class("BinarySwarm", 
            public = list(
              #' @description
              #' Creation of object of a Binary Swarm-based algorithm 
              #'
              #' @param population_size integer, size of the population
              #' @param n_bits integer, number of bits to be represented
              #' @param transferFun object of class `TransferFunction`
              #' @param particleGenerator object of class `BPG`
              initialize = function(population_size,
                                   n_bits, 
                                   transferFun, 
                                   particle_generator){
                # checks
                if(!("BPG" %in% class(particle_generator))){
                  stop("particle_generator should be of class 'BPG' ")
                }
                
                if(!("TransferFunction" %in% class(transferFun))){
                  stop("transferFun should be of class 'TransferFunction'")
                }
                
                if(n_bits != as.integer(n_bits)){
                  stop("n_bits should be an integer")
                }
                if(population_size != as.integer(population_size)){
                  stop("population_size should be an integer")
                }
                # end checks
                
                
                private$population_size <- population_size
                private$n_bits <- n_bits
                private$transferFun <- transferFun 
                
                # set the global best
                private$global_best <- list("Position" = 0, 
                                            "OptimResult" = 0)
                private$has_run <- FALSE
                private$iteration <- 1
                
                
                # particle_generator returns list of elements 
                private$population <- particle_generator$get(population_size, 
                                                            n_bits)
              },
              get_population = function(){
                return(private$population)
              }, 
              get_global_best = function(){
                if (!private$has_run){warning("Process hasn't run yet")}
                return(private$global_best)
              },
              #' @description
              #' Returns the iteration during the process
              #' Needed if the algorithm needs the iteration number
              #'
              #' @returns integer 
              get_iteration = function(){return(private$iteration)},
              #' @description
              #' Run the Swarm-based algorithm 
              #'
              #' @param fun function to maximize
              #' @param args_fun extra arguments for the function
              #' @param max_iter integer, 
              #' maximum number of iterations to take place
              #' @param max_stable integer, 
              #' maximum number of iterations the results needs to be stable to early stop the result
              #' @param seed a single value, interpreted as an integer, or NULL
              #'
              #' @returns list with 2 values `AllResults` and `BestResult`
              #' `BestResult` is list of 2 elements with the position and the result of the position 
              run_process = function(fun, 
                                     args_fun = list(), 
                                     max_iter = 30, 
                                     max_stable = 5, 
                                     seed = NULL){
                
                if(!is.null(seed)){set.seed(seed)} 
                
                private$has_run <- TRUE
                
                all_results <- list()
                counter_stable <- 0
                
                for(i in 1:max_iter){
                  # get all the results of the particles
                  results <- private$get_results(fun, 
                                                 args_fun)
                  
                  has_global_best <- private$set_global_best(results)
                  
                  # update All results
                  all_results <- append(all_results, 
                                       list(results))
                  
                  # keep a counter if the numbers are stable
                  counter_stable <- ifelse(has_global_best, 1, counter_stable + 1)

                  # it is stable enough, so get out the first loop
                  if(counter_stable >= max_stable){break}
                  
                  # update the positions of all particles
                  private$update_all_positions()
                  
                }
                return(list(AllResults = all_results, 
                            BestResult = private$global_best))
              }
            ),
            private = list(
              #' @field population_size integer, size of population
              population_size = NULL, 
              #' @field n_bits integer, number of bits to represent
              n_bits = NULL,
              #' @field transferFun object of class `TransferFunction` to update from continuous to binary
              transferFun = NULL, 
              #' @field global_best list with 2 elements with names `Position` and `OptimResult`
              global_best = NULL, 
              #' @field population list with `Particle` objects
              population = NULL, 
              #' @field has_run logical, if the process has been run
              has_run = NULL,
              #' @field iteration integer, position of the iteration during the process
              iteration = NULL, 
              update_all_positions = function(){
                 stop("Should be implemented in the subclass")
              }, 
              #' @description
              #' set the global best based on the results given 
              #'
              #' @param results list of results. 
              #' List of the result should have 3 elements
              #' `Positions`, `Results` & `OptimResults` 
              #' based on output of function [get_results()]
              #'
              #' @returns TRUE if the global best is updated
              #' FALSE when the global best did not change
               set_global_best = function(results){
                 changed_global <- FALSE
                 # update Global best
                 for(i in seq_along(results[["OptimResults"]])){
                   result <- results[["OptimResults"]][[i]]
                   position <- results[["Positions"]][[i]]
                   
                   if (is.null(private$global_best) ||
                       result > private$global_best[["OptimResult"]]) {
                     private$global_best <- list("Position" = position,
                                                "OptimResult" = result) 
                     changed_global <- TRUE
                   }
                 }
                 return(changed_global)
               }, 
              #' @description
              #' get the results of the function to maximize 
              #' 
              #' @param fun function to maximize, 
              #' with a binary sequence as first input.
              #' Function to maximize can give a list of results.
              #' Only the first element will be used to maximize. 
              #' This can be useful in case other results should to be monitored but should not optimized. 
              #' @param argsFun list with arguments passed to `fun`
              #'
              #' @returns list with 3 elements with names:
              #' `Results` list with all the output results 
              #' `Positions` list with all the positions checked
              #' `OptimResults` list with the results which should be optimized 
              #' The results and positions are in the same order of each other
               get_results = function(fun, argsFun){
                 results <- list() # results to optimize
                 positions <- list() # positions
                 all_result <- list() # all results that are given 
                 
                 for(part in private$population){
                   # first get the position
                   position <- part$get_position()
                   results_part <- do.call(fun, 
                                     args = append(list(position), argsFun))
                   # first element is to maximize
                   result <- results_part[[1]]
                   
                   #save result in the particle
                   part$save_result(result)
                   
                   # store the results
                   positions <- append(positions, list(position))
                   results <- append(results, list(result))
                   all_result <- append(all_result, list(results_part))
                 }
                 return(list("Results" = results, 
                             "Positions" = positions, 
                             "OptimResults" = all_result))
               } 
               )) -> BinarySwarm
