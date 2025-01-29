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
                                    seed = NULL){
                
                if (!("BPG-Velocity" %in% class(particle_generator))){
                  stop("Particle should be of class 'BPG-Velocity'")
                }
                
                super$initialize(population_size,
                                n_bits, 
                                transferFun, 
                                particle_generator, 
                                seed = seed)
                
                self$w <- w
                self$k1 <- k1
                self$k2 <- k2
              }
            ), 
            private = list(
              update_all_positions = function(){
                #loop through all particles
                for(particle in private$population){
                  rand <- runif(2)
                  position <- particle$get_position()
                  # particle 
                  velocity_next <- self$w * particle$get_velocity() +
                    self$k1 * rand[1] * (particle$get_personal_best()[["Position"]] - position) +
                    self$k2 * rand[1] * (self$get_global_best()[["Position"]] - position)
                  
                  position_next <- private$transferFun$changePosition(position, 
                                                                   velocity_next)
                  
                  particle$set_position_velocity(position_next, 
                                                 velocity_next)
                  
                  
                }
            })) -> SwarmBPSO

