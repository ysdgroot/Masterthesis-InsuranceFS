# Particle ----------------------------------------------------------------

R6::R6Class("ParticleBAOA", 
            inherit = Particle) -> ParticleBAOA


# Swarm for the particle --------------------------------------------------

R6::R6Class("SwarmBAOA", 
            inherit = BinarySwarm, 
            public = list(
              minMoa = NULL, 
              maxMoa = NULL, 
              beta = NULL, 
              k = NULL, 
              delta = NULL, 
              initialize = function(population_size,
                                    n_bits, 
                                    transferFun, 
                                    particle_generator, 
                                    beta, 
                                    k, 
                                    minMoa, 
                                    maxMoa,
                                    delta = 1e-8, 
                                    seed = seed){
                
                if (!("BPG" %in% class(particle_generator))){
                  stop("Particle should be of class 'BPG'")
                }
                
                super$initialize(population_size,
                                 n_bits, 
                                 transferFun, 
                                 particle_generator, 
                                 seed = seed)
                
                self$beta <- beta
                self$k <- k
                self$delta <- delta
                self$minMoa <- minMoa
                self$maxMoa <- maxMoa
              }
            ), 
            private = list(#' @description 
                           #' Math optimizer accelerated (MOA)
                           #' 
                           #' Function which is used the determine the probability of the exploration or exploitation phase 
                           #' 
                           #' @param t integer, current step of iterations 
                           #' @param minium,maximum numeric, minimum/maximum value of the MOA can be 
                           #' @param Tmax the maximum number of iterations
                           #'
                           #' @returns numeric value
                           MOA = function(t, Tmax){
                             return(self$minMoa + t * ((self$maxMoa - self$minMoa)/Tmax))
                           }, 
                           #' @description 
                           #' Math optimizer probability (MOP)
                           #' 
                           #' Additional value to determine the search
                           #'
                           #' @param t integer, current step of iterations 
                           #' @param Tmax the maximum number of iterations
                           #' @param beta numeric, sensitive parameter to define the accuracy of the phases 
                           #' The value is usually between 0 and 10
                           #'
                           #' @returns numeric value
                           MOP = function(t, Tmax){
                             return(1 - t ** (1/self$beta)/(Tmax**(1/self$beta)))
                           }, 
                           
                          #' @description
                          #'  Calculate the new positions 
                          #'
                          #' @param t integer, current step of iterations 
                          #' @param Tmax the maximum number of iterations
                          #' 
                          #' @details
                          #' @returns matrix with all the new positions. 
                          #' rows represents a particle. 
                          #' columns represents the positions in that dimension
                          #' @export
                           new_positions = function(t,
                                                    Tmax){
                            
                             # get MOA(t)
                             moa_t <- private$MOA(t, Tmax)
                             
                             # set the dimensions of the matrix 
                             n <- private$popSize
                             m <- private$nBits
                             
                             # exploration phase when r > MOA(t)
                             rand1 <- matrix(runif(n * m),
                                                nrow = n, 
                                                ncol = m)
                             
                             # vector of values 
                             isExploration <- rand1 > moa_t
                             
                             # generate random values for the type of calculation
                             rand2 <- matrix(runif(n * m),
                                            nrow = n, 
                                            ncol = m)
                             
                             # the MOP of at iteration t
                             mop_t <- private$MOP(t, Tmax)

                             # the original division is 
                             # (MOP + delta) * ((ub_j - lb_j) * k + lb_j)
                             # but because of binary levels ub_j = 1 and lb_j = 0
                             division <- (mop_t + private$delta) * private$k
                             
                             # original 
                             # MOP * ((ub_j - lb_j) * k + lb_j)
                             multi_add_minus <- mop_t * private$k
                             
                             # calculated everything at once 
                             # matrix addition and multiplication is iterating row and then column 
                             # not in the same 'shape' as the vector
                             matrix_global_best <- t(matrix(rep(private$globalBest$Position[[1]], 
                                                              private$popSize), 
                                                          ncol = private$popSize))
                             
                             result <- isExploration *
                               matrix_global_best *
                               ((rand2 < 0.5) * 1/division +
                                  (rand2 >= 0.5) * multi_add_minus) +
                               
                               !isExploration *
                               ( matrix_global_best +
                                  multi_add_minus *
                                  ((rand2 < 0.5) * (-1) +
                                     (rand2 >= 0.5)))
                             
                             return(result)
                           }, 
                          
                          update_all_positions = function(){
                            
                            t <- private$iteration
                            Tmax <- private$max_iteration
                            
                            # get MOA(t)
                            moa_t <- private$MOA(t, Tmax)
                            
                            # exploration phase when r > MOA(t)
                            rand1 <- matrix(runif(private$population_size * private$n_bits),
                                            nrow = private$population_size, 
                                            ncol = private$n_bits)
                            
                            # vector of values 
                            isExploration <- rand1 > moa_t
                            
                            # generate random values for the type of calculation
                            rand2 <- matrix(runif(private$population_size * private$n_bits),
                                            nrow = private$population_size, 
                                            ncol = private$n_bits)
                            
                            # the MOP of at iteration t
                            mop_t <- private$MOP(t, Tmax)
                            
                            # the original division is 
                            # (MOP + delta) * ((ub_j - lb_j) * k + lb_j)
                            # but because of binary levels ub_j = 1 and lb_j = 0
                            division <- (mop_t + self$delta) * self$k
                            
                            # original 
                            # MOP * ((ub_j - lb_j) * k + lb_j)
                            multi_add_minus <- mop_t * self$k
                            
                            # calculated everything at once 
                            # matrix addition and multiplication is iterating row and then column 
                            # not in the same 'shape' as the vector
                            matrix_global_best <- t(matrix(rep(private$global_best[["Position"]], 
                                                               private$population_size), 
                                                           ncol = private$population_size))
                            
                            result <- isExploration *
                              matrix_global_best *
                              ((rand2 < 0.5) * 1/division +
                                 (rand2 >= 0.5) * multi_add_minus) +
                              
                              !isExploration *
                              ( matrix_global_best +
                                  multi_add_minus *
                                  ((rand2 < 0.5) * (-1) +
                                     (rand2 >= 0.5)))
                            
                            i <- 1
                            # update position of each particle 
                            for(particle in private$population){
                              result_i <- result[i, ]
                              position <- particle$get_position()
                              new_position <- private$transferFun$changePosition(position, 
                                                                                 result_i)
                        
                              particle$set_position(new_position)
                              i <- i + 1
                            }
                            }
                           )) -> SwarmBAOA
