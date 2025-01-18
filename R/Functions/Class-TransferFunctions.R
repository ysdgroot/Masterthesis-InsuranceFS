#' Construction of the S-shaped and V-shaped transfer functions
#'
#' @field name character. Name of the transfer function
#' @field fun function. function to transform into values between 0 and 1
#' @field type character, "S" or "V", if it is S-shaped or V-shaped transfer class
#'
#' @returns R6 Class object of TransferFunction
#' @export
R6::R6Class("TransferFunction", 
            public = list(

              #' @param name character. Name of the transfer function
              #' @param fun function. function to transform into values between 0 and 1
              #' @param type character, "S" or "V", if it is S-shaped or V-shaped transfer class
              #'
              #' @returns R6 object 
              initialize = function(name, 
                                    fun, 
                                    type = "S"){
                private$name <- name 
                private$fun <- fun 
                
                if(!(type %in% c("S", "V"))){
                  stop(sprintf("Type should be 'S' or 'V' not %s", type))
                }
                private$type <- type
              }, 
              #' Transfer the values 
              #'
              #' @param x vector or matrix to calculate the function on
              #'
              #' @returns results of function `fun`
              transfer = function(x){
                  return(private$fun(x))
                }, 
              
              #' Update the next positions based on the transfer functions
              #'
              #' @param x binary vector of current position
              #' @param velocity vector of the next position which is also called the velocity. 
              #' based on the velocity the next binary positions are produced  
              #'
              #' @returns transfer the final results
              changePosition = function(x, velocity){
                rand <- runif(length(x))
                if("matrix" %in% class(x)){
                  rand <- matrix(rand, 
                                 nrow = dim(x)[1], 
                                 ncol = dim(x)[2])
                } 
                
                transferedy <- self$transfer(velocity)
                if(private$type == "S"){
                  return( 1 * (rand < transferedy))
                } else if(private$type == "V"){
                  return(1 * ((rand < transferedy) * abs(x - 1) +
                           (rand >= transferedy) * x))
                } else{ 
                  stop("Type is not correct")  
                }
              }
              
            ), 
            private = list(name = NULL,
                           fun = NULL, 
                           type = NULL)) -> TransferFunction
