

# creation of the swarm

library(sigmoid)

binaryParticle <- setRefClass("binaryParticle", 
                        fields = list(position = "numeric", 
                                      velocity = "numeric", 
                                      results = "numeric", 
                                      bestresult = "numeric"), 
                        methods = list(
                          initialize = function(position, velocity){
                            .self$position <- position 
                            .self$velocity <- velocity 
                            .self$bestresult <- NA
                          }, 
                          saveResult = function(result){
                            .self$result <- c(.self$result, result)
                            
                            if(is.na(.self$bestresult)){
                              .self$bestresult <- result
                            } else {
                              .self$bestresult <- max(.self$bestresult, result)
                            }
                          }, 
                          getPersonalBest = function(){
                           return(.self$bestresult) 
                          }, 
                          updatePositionVelocity = function(newPos, newVel){
                            .self$position <- newPos 
                            .self$velocity <- newVel 
                          }, 
                      ))


binarySwarm <- setRefClass("binarySwarm", 
                           fields = list(nParticles = "numeric", 
                                         length = "numeric", 
                                         listParticles = "list", 
                                         listResults = "list"))

binarySwarm$methods(
  initialize = function(nParticles, 
                        length, 
                        chanceBit = 0.2,
                        boundaryVelocity = c(-6,6)){
    .self$nParticles <- nParticles
    .self$length <- length
    
    # random initialization of the particles
    listParticles <- list()
    for (i in 1:nParticles){
      randPosition <- as.numeric(runif(length) <= chanceBit)
      randVelocity <- runif(length, 
                            min = min(boundaryVelocity), 
                            max = max(boundaryVelocity))
      particle <- binaryParticle$new(position = randPosition, 
                                     velocity = randVelocity)
      listParticles <- append(listParticles, particle)
    }
    
    .self$listParticles <- listParticles
  }, 
  getGlobalBest = function(){
    #TODO: check function -- not sure if it works like this
    return(max(lapply(.self$listParticles, \(x) x$getPersonalBest())))
  },
  getResults = function(fun){
    results <- c()
    positions <- c()
    
    #TODO: improve the implementation by saving the results and retreiving them
    for(part in .self$listParticles){
      position <- part$position
      result <- fun(position)
      
      # save results in BinaryParticle
      part$saveResult(result)

      positions <- c(positions, position)
      results <- c(results, result)
    }
    return(list("Results" = results, 
                "Positions" = positions))
      
  },
  
  BPSO = function(fun, 
                  inertia, 
                  cPersonal, 
                  cGlobal,
                  transferFunction,
                  maxiter = 10, 
                  maxStable = 5){
    
    allResults <- list()
    bestResult <- list("Position" = 0, 
                       "Result" = 0)
    counterStable <- 0
    if(!is(t, "transferFunction")){
      stop("Given transferFunction is not a tranferFrunction (ReferenceClass)")
    }
    
    for(i in 1:maxiter){
      # get all the 
      results <- .self$getResults(fun)
      
      if (max(results$Result) > bestResult$Result){
        bestResult$Result <- results$Result
        bestResult$Position <- results$Position
        counterStable <- 1
      } else {
        counterStable <- counterStable + 1
      }
      
      if(counterStable >= maxStable){
        # it is stable enough, so get out the first loop
        break
      }
      
      # update positions and velocities
      j <- 1 
      for(part in .self$listParticles){
        rand <- runif(2)
        
        # calculate the new velocity of the point
        newVel <-inertia*part$velocity +
          rand[1] * cPersonal * (part$getPersonalBest() - results[j]) +
          rand[2] * cGlobal * (.self$getGlobalBest() - results[j])
          
        transformedNewVel <- transferFunction$transfer(newVel)
        newPosition <- transferFunction$changePosition(part$position,
                                                       newVel)
        part$updatePositionVelocity(newPos = newPosition, 
                                    newVel = newVel)
        
        j <- j + 1
      }
    }
    
    return(bestResult)
  }
  
)

