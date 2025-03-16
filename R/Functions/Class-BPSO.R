# Particle of the swarm ---------------------------------------------------

R6::R6Class("ParticleBPSO", 
            inherit = ParticleVelocity) -> ParticleBPSO

# Swarm -------------------------------------------------------------------

R6::R6Class("SwarmBPSO",
            inherit = BinarySwarm, 
            public = list(
              w = NULL, 
              k1 = NULL, 
              k2 = NULL, 
              initialize = function(population_size,
                                    n_bits, 
                                    transferFun, 
                                    particle_generator, 
                                    w, 
                                    k1, 
                                    k2, 
                                    use_var_importance = FALSE, 
                                    seed = NULL){
                
                if (!("BPG-Velocity" %in% class(particle_generator))){
                  stop("Particle should be of class 'BPG-Velocity'")
                }
                
                super$initialize(population_size,
                                n_bits, 
                                transferFun, 
                                particle_generator, 
                                use_var_importance = use_var_importance,
                                seed = seed)
                
                self$w <- w
                self$k1 <- k1
                self$k2 <- k2
              }
            ), 
            private = list(
              update_all_positions = function(){
                #loop through all particles
                list_var_importance <- list()
                for(particle in private$population){
                  rand <- runif(2)
                  position <- particle$get_position()
                  # particle 
                  velocity_next <- self$w * particle$get_velocity() +
                    self$k1 * rand[1] * (particle$get_personal_best()[["Position"]] - position) +
                    self$k2 * rand[1] * (self$get_global_best()[["Position"]] - position)
                  
                  if(super$get_use_variable_importance()){
                    next_values <- private$transferFun$changePosition(position, 
                                                                      velocity_next, 
                                                                      use_var_importance = TRUE)
                    
                    position_next <- next_values[[1]]
                    list_var_importance <- append(list_var_importance, 
                                                  next_values[["TransferValues"]])
                    
                  } else {
                    position_next <- private$transferFun$changePosition(position, 
                                                                        velocity_next, 
                                                                        use_var_importance = FALSE)
                  }
                  
                  particle$set_position_velocity(position_next, 
                                                 velocity_next)
                  
                  
                }
                if(super$get_use_variable_importance()){self$add_variable_importance(list_var_importance)}
            })) -> SwarmBPSO

