# For Binary Particle Swarm Algorithm
#
#
library(sigmoid)

# continuous 
nextPositionVelocity <- function(position,
                         velocity,
                          inertia, 
                          personalBest, 
                          generalBest,
                          constantPersonal = 0.5, 
                          constantGeneral = 0.5){
  
  #TODO: check if position and velocity are vectors (of the same length)
  
  #TODO: 2 random numbers(vectors) 
  rand1 <- 0.3
  rand2 <- 0.4
  
  nextVelocity <- inertia * velocity + 
    constantPersonal * rand1 * (personalBest - position) +
    constantPersonal * rand2 * (generalBest - position)
  
  nextPosition <- position + nextVelocity
  
  return(list("Position" = nextPosition, 
              "Velocity" = nextVelocity))
}

# binary 
nextPositionVelocity <- function(position,
                                 velocity,
                                 inertia, 
                                 personalBest, 
                                 generalBest,
                                 constantPersonal = 0.5, 
                                 constantGeneral = 0.5){
  
  #TODO: check if position and velocity are vectors (of the same length)
  
  #TODO: 1 random numbers (in paper between 0.1-1)
  rand1 <- 0.3
  rand2 <- 0.4
  
  
  nextVelocity <- inertia * velocity + 
    constantPersonal * rand1 * (personalBest - position) +
    constantPersonal * rand2 * (generalBest - position)
  
  rand3 <- 0.6
  
  # get sigmoid -- for the standard BPSO 
  sVelocity <- sigmoid(nextVelocity)
  
  nextPosition <- as.numeric(rand3 < sVelocity)
  
  return(list("Position" = nextPosition, 
              "Velocity" = nextVelocity))
}

velocityTransformBPSO <- function(velocity){
  return(sigmoid(velocity))
}

velocityTransformNBPSO <- function(velocity){
  return(sigmoid(velocity))
}

positionTransformBPSO <- function(position, 
                                  transformedVelocity){
  
  #TODO: make it a random number 
  rand3 <- 0.6
  
  nextPosition <- as.numeric(rand3 < transformedVelocity)
  return(nextPosition)
}

positionTransformNBPSO <- function(position, 
                                   transformedVelocity){
  
  #TODO: make it a random number 
  rand3 <- 0.6
  
  nextPosition <- (rand3 < transformedVelocity) * abs(position - 1) +
    (rand3 >= transformedVelocity) * position
  
  return(nextPosition)
}


#TODO: implement the INBPSO -- zou betere resultaten opleveren

