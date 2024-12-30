library(R6)

# Particle of the swarm ---------------------------------------------------

R6::R6Class("BinaryParticle", 
            public = list(
              initialize = function(position, 
                                    velocity){
                
                private$position <- position
                private$velocity <- velocity
                
                # set a record for all the changes that happened with the result
                private$track <- list(list(Position = position, 
                                            Velocity = velocity, 
                                            Result = 0))
              }, 
              saveResult = function(result){
                # save result 
                private$track[[length(private$track)]][["Result"]] <- result
                
                # update PersonalBest

                if(is.null(private$bestResult)){
                  private$bestResult <- result
                } else {
                  private$bestResult <- max(private$bestResult, result)
                }
              }, 
              getPersonalBest = function(){
                return(private$bestResult)
              },
              getPosition = function(){
                return(private$position)
              }, 
              getVelocity = function(){
                return(private$velocity)
              }, 
              getPositionVelocity = function(){
                return(list(Position = private$position,
                            Velocity = private$velocity))
              },
              setPositionVelocity = function(position, velocity){
                
                private$track <- append(private$track, 
                                        list(list(Position = position, 
                                                  Velocity = velocity, 
                                                  Result = 0)))
                
                private$position <- position
                private$velocity <- velocity
              }, 
              getTrack = function(){
                return(private$track)
              },
              updatePositionVelocity = function(w, 
                                                k1, 
                                                k2,
                                                r1, 
                                                r2, 
                                                globalBest, 
                                                transferFunction){
                velocity_next <- w * private$velocity +
                  k1 * r1 * (self$getPersonalBest() - private$position) +
                  k2 * r2 * (globalBest - private$position)
                
                position_next <- transferFunction$changePosition(private$position, 
                                                                 velocity_next)
                
                self$setPositionVelocity(position_next, velocity_next)
              }
            ), 
            private = list(position = NULL, 
                           velocity = NULL, 
                           bestResult = NULL, 
                           track = NULL)) -> BinaryParticle

# Swarm -------------------------------------------------------------------

R6::R6Class("BinarySwarm", 
            public = list(
              initialize = function(popSize, 
                                    nBits, 
                                    w, 
                                    k1, 
                                    k2,
                                    transferFun, 
                                    suggestions = NULL, 
                                    chanceBit = 0.2, 
                                    boundaryVelocity = c(-6,6), 
                                    seed = NULL){
                
                if(!is.null(seed)){set.seed(seed)} 
                
                private$popSize <- popSize
                private$nBits <- nBits
                private$w <- w
                private$k1 <- k1
                private$k2 <- k2
                private$transferFun <- transferFun
                private$hasRun <- FALSE
                private$globalBest <- list("Position" = 0, 
                                           "Result" = 0)
                
                # Construction of the particles
                listParticles <- list()
                if (!is.null(suggestions)){
                  part_list <- sample(suggestions, 
                                      size = min(popSize, length(suggestions)))
                  
                  for(element in part_list){
                    position <- element[1]
                    velocity <- element[2]
                    
                    particle <- BinaryParticle$new(position = position, 
                                                   velocity = velocity)
                    listParticles <- append(listParticles, particle)
                  }
                }
                
                # random initialization of the particles when not enough suggestions
                if(popSize > length(suggestions)){
                  for (i in 1:(popSize - length(suggestions))){
                    randPosition <- as.numeric(runif(nBits) <= chanceBit)
                    randVelocity <- runif(nBits, 
                                          min = min(boundaryVelocity), 
                                          max = max(boundaryVelocity))
                    particle <- BinaryParticle$new(position = randPosition, 
                                                    velocity = randVelocity)
                    listParticles <- append(listParticles, particle)
                  }
                }
                
                private$Population <- listParticles
              }, 
              runProcess = function(fun,
                                    argsFun =list(), 
                                    maxIter = 30, 
                                    maxStable = 5, 
                                    seed = NULL){
                
                if(!is.null(seed)){set.seed(seed)} 
                
                private$hasRun <- TRUE
                
                allResults <- list()
                counterStable <- 0
 
                for(i in 1:maxIter){
                  # get all the results of the particles
                  results <- private$getResults(fun, 
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
                  
                  # update positions and velocities
                  for(part in private$Population){
                    # random numbers
                    rand <- runif(2)
                    
                    # the particle takes care of the update of the 
                    part$updatePositionVelocity(w = private$w, 
                                                k1 = private$k1, 
                                                k2 = private$k2, 
                                                r1 = rand[1],
                                                r2 = rand[2], 
                                                globalBest = private$globalBest$Position[[1]], 
                                                transferFunction = private$transferFun)
                  }
                }
                return(list(AllResults = allResults, 
                            BestResult = private$globalBest))
              }, 
              getPopulation = function(){
                return(private$Population)
              }, 
              getGlobalBest = function(){
                if (!private$hasRun){stop("Process hasn't run yet")}
                return(private$globalBest)
              }
            ), 
            private = list(globalBest = NULL, 
                           Population = NULL, 
                           popSize = NULL, 
                           nBits = NULL, 
                           w = NULL,
                           k1 = NULL, 
                           k2 = NULL,
                           transferFun = NULL,
                           hasRun = NULL,
                           results = NULL,
                           saveResult = function(result){
                             # update Global best
                             
                             if(is.null(private$globalBest)){
                               private$globalBest <- result
                             } else {
                               private$globalBest <- max(private$globalBest, result)
                             }
                             
                           }, 
                           getResults = function(fun, argsFun){
                             results <- list()
                             #TODO: positions are not good
                             positions <- list()
                             
                             for(part in private$Population){
                               position <- part$getPosition()
                               result <- do.call(fun, 
                                                 args = append(list(position), argsFun))
                               
                               # save results in BinaryParticle
                               part$saveResult(result)
                               
                               positions <- append(positions, list(position))
                               results <- append(results, list(result))
                             }
                             return(list("Results" = results, 
                                         "Positions" = positions))
                           })) -> BinarySwarm



