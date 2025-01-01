library(R6)
#TODO: documentation
R6::R6Class("BAOA", 
            public = list(
              initialize = function(popSize, 
                                    nBits, 
                                    beta, 
                                    k, 
                                    minMoa, 
                                    maxMoa, 
                                    transferFun, 
                                    suggestions = NULL, 
                                    chanceBit = 0.2,
                                    delta = 1e-8){
                private$popSize <- popSize
                private$nBits <- nBits
                private$beta <- beta
                private$k <- k
                private$delta <- delta
                private$transferFun <- transferFun
                private$minMoa <- minMoa
                private$maxMoa <- maxMoa
                private$globalBest <- list("Position" = 0, 
                                           "Result" = 0)
                private$hasRun <- FALSE
                
                
                # Construction of the particles
                list_elements <- list()
                if (!is.null(suggestions)){
                  part_list <- sample(suggestions, 
                                      size = min(popSize, length(suggestions)))
                  
                  for(element in part_list){
                    position <- element
                    list_elements <- append(list_elements, list(position))
                  }
                }
                
                # random initialization of the particles when not enough suggestions
                if(popSize > length(suggestions)){
                  for (i in 1:(popSize - length(suggestions))){
                    randPosition <- as.numeric(runif(nBits) <= chanceBit)

                    list_elements <- append(list_elements, list(randPosition))
                  }
                }
                
                private$population <- list_elements
                
              }, 
              
              run_process = function(fun, 
                                     argsFun = list(), 
                                     maxIter = 30, 
                                     maxStable = 10, 
                                     seed = NULL){
                if(!is.null(seed)){set.seed(seed)} 
                 
                private$hasRun <- TRUE
                
                allResults <- list()
                counterStable <- 0
                
                for(i in 1:maxIter){
                  # get all the results of all positions 
                  results <- private$get_results(fun, 
                                                argsFun)
                  
                  # update All results
                  allResults <- append(allResults, list(results))
                  
                  if (max(unlist(results$Results)) > private$globalBest$Result){
                    
                    private$globalBest$Result <- max(unlist(results$Results))
                    private$globalBest$Position <- results$Position[min(which(results$Results == max(unlist(results$Results))))]
                    counterStable <- 1
                  } else {
                    counterStable <- counterStable + 1
                  }
                  
                  # it is stable enough, so get out the first loop
                  if(counterStable >= maxStable){break}
                  
                  # update the different phase
                  next_pos <- private$new_positions(i, 
                                                    maxIter)
                  
                  # transfer the to Binary setting (velocity is the new position)
                  next_pos_trans <- private$transferFun$changePosition(next_pos, next_pos)
                  
                  t_mat <- t(next_pos_trans)
                  next_pos_trans <- split(t_mat, 
                        rep(1:ncol(t_mat), 
                            each = nrow(t_mat)))
                  names(next_pos_trans) <- NULL #otherwise there will be names added
                  
                  # update the positions to be 
                  private$population <- next_pos_trans
                }
                
                return(list(AllResults = allResults, 
                            BestResult = private$globalBest))

              }, 
              getGlobalBest = function(){
                if (!private$hasRun){stop("Process hasn't run yet")}
                return(private$globalBest)
              }
            ), 
            private = list(popSize = NULL, 
                           population = NULL, 
                           nBits = NULL, 
                           beta = NULL, 
                           k = NULL, 
                           delta = NULL, 
                           transferFun = NULL, 
                           minMoa = NULL, 
                           maxMoa = NULL,
                           globalBest = NULL,
                           hasRun = NULL,
                           #' @description 
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
                             return(private$minMoa + t * ((private$maxMoa - private$minMoa)/Tmax))
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
                             return(1 - t ** (1/private$beta)/(Tmax**(1/private$beta)))
                           }, 
                           
                          #' @description
                          #'  Calculate the new positions 
                          #'
                          #' @param t integer, current step of iterations 
                          #' @param Tmax the maximum number of iterations
                          #' 
                          #' @details
                          #' 
                          #' 
                          #'
                          #' @returns
                          #' @export
                          #'
                          #' @examples
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
                           get_results = function(fun, argsFun){
                             results <- list()
                             positions <- list()
                             for(part in private$population){
                               # part is in this only a position
                               position <- part
                               result <- do.call(fun, 
                                                 args = append(list(position), argsFun))
                               
                               positions <- append(positions, list(position))
                               results <- append(results, list(result))
                             }
                             return(list("Results" = results, 
                                         "Positions" = positions))
                           }
                           )) -> BAOA