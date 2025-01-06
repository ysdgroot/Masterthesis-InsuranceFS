# ParticleGenerator -------------------------------------------------------

#' @title Binary Particle Generator (BPG)
#' 
#' @description 
#' Abstract class for a wrapper making particles for a Binary Swarm
#' 
R6::R6Class("BPG", 
            public = list(
              #' @field chance_bit numeric between 0 and 1
              #' chance to have 1 in the binary construction
              chance_bit = NULL, 
              #' @field suggestions NULL or list with possible positions 
              suggestions = NULL, 
              
              #' @description
              #' Creation of a Binary Particle Generator
              #'
              #' @param generator `R6ClassGenerator` for something of class `Particle`  
              #' @param chance_bit numeric between 0 and 1 (not included)
              #' chance to have 1 in the binary construction
              #' @param suggestions NULL or list with possible starting positions 
              #'
              #' @returns
              initialize = function(generator, 
                                    chance_bit = 0.5, 
                                    suggestions = NULL){
                if(!("Particle" %in% generator$inherit)){
                  #TODO: check for inherit from different levels
                  stop("Generator should inherit from Particle")
                }
                if(chance_bit >= 1 || chance_bit <= 0){
                  stop("Value for chance_bit should be in interval (0, 1)")
                }
                
                private$generator <- generator
                self$suggestions <- suggestions
                self$chance_bit <- chance_bit
              },
              #' @description
              #' Creation function for the particles
              #' 
              #' @param size integer, number of Particles that needs to be generated
              #' @param n_bits integer, number of bits to be represented
              #' @param seed NULL or integer, for [set.seed()]
              #'
              #' @returns list with selected (`suggestions`) and/or generated particles
              get = function(size, 
                             n_bits,
                             seed = NULL){
                if(!is.null(seed)){set.seed(seed)}
                
                # Construction of the particles
                listParticles <- list()
                if (!is.null(self$suggestions)){
                  # check
                  good_suggestions <- self$check_suggestions(suggestions = suggestions, 
                                                              n_bits = n_bits)
                  
                  if(!good_suggestions){
                    stop("Given suggestions are not good for the given n_bits")
                  }
                  
                  # select first from the suggestions
                  part_list <- sample(self$suggestions, 
                                      size = min(size, length(self$suggestions)))
                  
                  for(element in part_list){
                    # take the first element 
                    #TODO: check if this is correct to take the position
                    position <- element[[1]]
                    
                    particle <- private$generator$new(position)
                    listParticles <- append(listParticles, particle)
                  }
                }
                
                # random initialization of the particles when not enough suggestions
                if(size > length(self$suggestions)){
                  for (i in 1:(size - length(self$suggestions))){
                    randPosition <- as.numeric(runif(n_bits) <= self$chance_bit)
                    
                    particle <- private$generator$new(position = randPosition)
                    listParticles <- append(listParticles, particle)
                  }
                }
                return(listParticles)
              }, 

            #' @description
            #' check if all the suggestions can be accepted
            #'
            #' @param suggestions NULL or list with possible starting positions
            #' @param n_bits integer, number of bits to be represented
            #'
            #' @returns logical (`logical(1)`) if all suggestions can be accepted
            check_suggestions = function(suggestions, 
                                         n_bits){
              all_ok <- TRUE 
              for(suggestion in suggestions){
                position <- suggestion[[1]]
                if(length(position) != n_bits){
                  message(sprintf("Length of a suggestion should be %d, not %d", 
                               n_bits, 
                               length(position)))
                  all_ok <- FALSE
                  break
                }
              }
              return(all_ok)
            }
            ), 
            private = list(generator = NULL)
            ) -> BPG

#' @title Binary Particle Generator-Velocity (BPG-Velocity)
#' 
#' @description 
#' Abstract class for a wrapper making particles for a Binary Swarm with Velocity
#' 
R6::R6Class("BPG-Velocity", 
            inherit = BPG, 
            public = list(
              #' @field chance_bit numeric between 0 and 1
              #' chance to have 1 in the binary construction
              chance_bit = NULL, 
              #' @field suggestions NULL or list with possible positions. 
              #' Each element of the list should have 2 different elements 
              #' First one is for the (binary) position, the second for the velocity 
              #' Each position and velocity should have length n_bits, see [get()]
              suggestions = NULL, 
              #' @field boundary_velocity numeric (2 values), 
              #' describing the boundaries of the velocity
              boundary_velocity = NULL, 
              initialize = function(generator, 
                                    chance_bit = 0.5, 
                                    boundary_velocity = c(-6, 6),
                                    suggestions = NULL){
                # call the super 
                super$intialize(generator, 
                                chance_bit, 
                                suggestions)
                
                if(!("Particle-Velocity" %in% generator$inherit)){
                  #TODO: check for inherit from different levels
                  stop("Generator should inherit from Particle")
                }
                
                self$boundary_velocity <- boundary_velocity
              },
              #' @description
              #' Creation function for the particles
              #' 
              #' @param size integer, number of Particles that needs to be generated
              #' @param n_bits integer, number of bits to be represented
              #' @param seed NULL or integer, for [set.seed()]
              #'
              #' @returns list with selected (`suggestions`) and/or generated particles
              get = function(size, n_bits, seed = NULL){
                if(!is.null(seed)){set.seed(seed)}
                
                # Construction of the particles
                listParticles <- list()
                if (!is.null(self$suggestions)){
                  # check
                  good_suggestions <- self$check_suggestions(suggestions = suggestions, 
                                                             n_bits = n_bits)
                  
                  if(!good_suggestions){
                    stop("Given suggestions are not good for the given n_bits")
                  }
                  
                  part_list <- sample(self$suggestions, 
                                      size = min(size, length(self$suggestions)))
                  
                  for(element in part_list){
                    position <- element[[1]]
                    velocity <- element[[2]]
                    

                    particle <- private$generator$new(position, 
                                                      velocity)
                    listParticles <- append(listParticles, particle)
                  }
                }
                
                # random initialization of the particles when not enough suggestions
                if(size > length(self$suggestions)){
                  for (i in 1:(size - length(self$suggestions))){
                    randPosition <- as.numeric(runif(n_bits) <= self$chance_bit)
                    randVelocity <- runif(n_bits, 
                                          min = min(self$boundary_velocity), 
                                          max = max(self$boundary_velocity))
                    
                    particle <- private$generator$new(randPosition, 
                                                      randVelocity)
                    listParticles <- append(listParticles, particle)
                  }
                }
                return(listParticles)
              }, 
              
              #' @description
              #' check if all the suggestions can be accepted
              #'
              #' @param suggestions NULL or list with possible starting positions
              #' @param n_bits integer, number of bits to be represented
              #'
              #' @returns logical (`logical(1)`) if all suggestions can be accepted
              check_suggestions = function(suggestions, 
                                           n_bits){
                all_ok <- TRUE 
                for(suggestion in suggestions){
                  if(length(suggestion) != 2){
                    message("Length of each suggestion should be 2, first for position then velocity")
                    all_ok <- FALSE 
                    break
                  }
                  position <- suggestion[[1]]
                  velocity <- suggestion[[2]]
                  
                  if(length(position) != n_bits || length(velocity) != n_bits){
                    message("Length of the first and second element should be the same length")
                    all_ok <- FALSE
                    break
                  } 
                }
                return(all_ok)
              }
              ), 
            private = list(
              generator = NULL
              )
            ) -> BPG_Velocity
