

#' Function to help for the split in the Feature Selection methodology
#'
#' @param object ga object of the GA package
#' @param ... not used
#' @param maxBits maximum number of bits to have in the GA 
#' @param size the number of splits of the current bitstring. 
#' Random bits will be selected of size of maxBits  
#'
#' @return
#' @export
bitSelection <- function(object,
                         ...,
                         maxBits = 20, 
                         size = 5){
  
  sizePop <- nrow(object@population)
  listFitness <- c()
  nextPopulation <- matrix(nrow = 1, 
                           ncol = object@nBits)
  
  k <- 1 # pointer to the location
  for(i in seq_len(sizePop)){
    pop <- object@population[i,]
    nBitsUsed <- sum(pop)
    if(nBitsUsed > maxBits){
      # positions with 1 
      positions <- which(pop == 1)
      totalSample <- matrix(nrow = size, 
                            ncol = object@nBits)
      # random sample of those 
      for (j in 1:size){
        nextPositions <- sample(positions, 
                                size = maxBits, 
                                replace = FALSE)
        nextSample <- rep(0, object@nBits)
        nextSample[nextPositions] <- 1
        totalSample[j,] <- nextSample
      }
      # only select unique rows 
      totalSample <- unique(totalSample)
      nextPopulation <- rbind(nextPopulation, totalSample)
      
      nrowAdded <- nrow(totalSample)
      listFitness[k:(k + nrowAdded - 1)] <- NA
      
      k <- k + nrowAdded
      
    } else {
      nextPopulation <- rbind(nextPopulation, pop)
      listFitness[k] <- object@fitness[i]
      k <- k + 1
    }
    
  }
  #remove the NA from the first row
  nextPopulation <- nextPopulation[2:nrow(nextPopulation),]
  
  rownames(nextPopulation) <- NULL
  
  return(list("Pop" = nextPopulation, 
              "Fitness" = listFitness))
  
}