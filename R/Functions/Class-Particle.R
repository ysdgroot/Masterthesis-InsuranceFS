#' @title Particle
#'
#' @description 
#' Abstract class for a particle
R6::R6Class("Particle", 
            public = list(
              #' @field position vector with only 0 or 1
              initialize = function(position){
                
                if(length(setdiff(unique(position), c(0, 1))) != 0){
                  stop("Position should only have 0 or 1 values")
                }
                
                private$position <- position 
                private$track <- list()
              }, 
              #' @description
              #' save the result of the current position
              #'
              #' @param result a single numeric value
              #'
              #' @returns returns nothing
              save_result = function(result){
                
                add_track <- list("Position" = private$position, 
                                  "Result" = result)
                
                private$track <- append(private$track, 
                                        list(add_track))
                
                if(is.null(private$personal_best) ||
                   private$personal_best[["Result"]] < result){
                  private$personal_best <- add_track
                } 
                
              }, 
              #' @description
              #' return the best result of the particle
              #' 
              #' @returns list with 2 elements with names 
              #' `Position` and `Result`
              get_personal_best = function(){
                return(private$personal_best)
              }, 
              #' @description
              #' return the current position of the particle
              #' 
              #' @returns position, vector of a length
              get_position = function(){
                return(private$position)
              }, 
              #' @description
              #' return the current position of the particle
              #' 
              #' @returns position, vector of a length
              set_position = function(position){
                if(length(position) != length(private$position)){
                  stop("Different length compared to current position")
                }
                if(!private$check_position(position)){
                  stop("New position should only have 0 or 1")
                }
                private$position <- position
              }, 
              #' @description
              #' returns list of the history of this particle
              #' 
              #' @returns list of lists with 2 elements with names 
              #' `Position` and `Result`
              get_track = function(){
                return(private$track)
              }, 
              
              #' @description
              #' Get string with the current position#' 
              #'
              #' @returns string of length `length(position)`
              get_char_position = function(){
                return(paste(private$position, collapse = ""))
              }
            ), 
            private = list(
              position = NULL, 
              track = NULL, 
              personal_best = NULL, 
              #' @description
              #' check if the new position only has 0 and 1
              #' @param position vector to be checked
              #'
              #' @returns `logical(1)` TRUE if the position only contains 1 and/or 0 
              check_position = function(position){
                return(length(setdiff(unique(position), c(0, 1))) == 0)
              }
            )) -> Particle

#' @title ParticleVelocity
#'
#' @description 
#' Abstract class for a particle with velocity. 
#' This inherits the functionality of a simple particle
R6::R6Class("ParticleVelocity", 
            inherit = Particle, 
            public = list(
              initialize = function(position, 
                                    velocity){
                super$initialize(position)
                
                if(length(position) != length(velocity)){
                  stop("Position and Velocity should have the same length")
                }
                
                private$velocity <- velocity
              }, 
              
              #' @description
              #' Get the current Velocity 
              #'
              #' @returns vector of fixed length 
              get_velocity = function(){
                return(private$velocity)
              }, 
              #' @description
              #' Set the current velocity into a new one
              #'
              #' @returns NULL 
              set_velocity = function(velocity){
                if(length(velocity) != length(private$velocity)){
                  stop("Different length compared to current velocity")
                }
                private$velocity <- velocity
              }, 
              #' @description
              #' Set the position and velocity into a new one
              #'
              #' @returns NULL 
              set_position_velocity = function(position, 
                                               velocity){
                super$set_position(position)
                self$set_velocity(velocity)
              }
            ), 
            private = list(
              velocity = NULL
            )) -> ParticleVelocity



