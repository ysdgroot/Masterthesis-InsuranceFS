#' Construction of the S-shaped and V-shaped transfer functions
#'
#' @field name character. 
#' @field fun function. 
#' @field type character. 
#'
#' @returns
#' @export
transferFunction <- setRefClass("transferFunction", 
                                fields = list(name = "character",
                                              fun = "function", 
                                              type = "character"), 
                                methods = list(
                                  initialize = function(name, fun, type = "S"){
                                    .self$name <- name 
                                    .self$fun <- fun 
                                    
                                    if(!(type %in% c("S", "V"))){
                                      stop(sprintf("Type should be 'S' or 'V' not %s", type))
                                    }
                                    .self$type <- type
                                  }, 
                                  transfer = function(x){
                                    max_val <- max(x)
                                    min_val <- min(x)
                                    
                                    if(max_val > 1 || min_val < 0){
                                      stop("The values of x should 0 or 1")
                                    }
                                    
                                    return(.self$fun(x))}, 
                                  changePosition = function(x, velocity){
                                    rand <- runif(length(x))
                                    transferedy <- .self$transfer(velocity)
                                    if(.self$type == "S"){
                                      return(rand < transferedy)
                                    } else if(.self$type == "V"){
                                      return((rand < transferedy) * abs(x - 1) +
                                               (rand >= transferedy) * x)
                                    } else{ 
                                      stop("Type is not correct")  
                                    }
                                  }
                                ))

baseClassTransferFunctions <- list(S1 = list(FUN = function(x){sigmoid(x)}, 
                                               type = "S"), 
                                   S2 = list(FUN = function(x){sigmoid(x/2)}, 
                                               type ="S"), 
                                   S3 = list(FUN = function(x){sigmoid(2*x)}, 
                                               type ="S"), 
                                   V1 = list(FUN = function(x){abs(x)/(sqrt(1 + x**2))}, 
                                             type = "V"),
                                   V2 = list(FUN = function(x){abs(tanh(x))}, 
                                             type = "V"),
                                   V3 = list(FUN = function(x){abs(2*atan(pi * x /2)/pi)}, 
                                             type = "V"))

#TODO: transform the list into the different objects 

t <- transferFunction$new("S1",baseClassTransferFunctions$S1$FUN,baseClassTransferFunctions$S1$type)
