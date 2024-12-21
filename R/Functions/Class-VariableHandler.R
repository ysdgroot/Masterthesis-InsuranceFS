library(methods)

# setRefClass returns a generator  
variableHandler <- setRefClass("variableHandler", 
                      fields = list(variables = "character", 
                                    order = "numeric", 
                                    all_variables = "character", 
                                    length = "numeric"))
variableHandler$methods(
  initialize = function(variables, order = 1){
    # set default values
    .self$variables <- sort(variables)
    .self$order <- order 
    
    # check on the variable order
    if(order < 1 | order%%1 != 0){
      stop("value of m should be equal or higher than 2 and a whole number")
    }
    
    # get all the combinations for each order
    full_list <- c()
    for (m in 1:order){
      combinations <- combn(variables, m = m, simplify = FALSE) |> 
        lapply(FUN = \(x)(paste(x, collapse = ":"))) |> 
        unlist()
      
      full_list <- c(full_list, combinations)
    }
    
    .self$all_variables <- full_list
    .self$length <- length(full_list)
  }, 
  getVariables = function(coding, withMain = FALSE){
    
    if(length(coding) != .self$length){
      stop(sprintf("Length of the coding is incorrect, it should have length %d", .self$length))
    }
    
    if(!all(coding %in% c(0,1))){
      stop("It should only contain 0 and 1")
    }
    
    selected_coding <- coding
    if (withMain){
      # update the coding based on the function
      selected_coding <- .self$getCoding(variables = .self$all_variables[coding == 1], 
                                         withMain = TRUE)
    }
    
    return(.self$all_variables[selected_coding == 1])
  }, 
  getCoding = function(variables, withMain = FALSE){
    
    if(!all(variables %in% .self$all_variables)){
      stop("Not all given variables exists")
    }
    
    variables_select <- unique(variables)
    
    if(withMain){
      main_variables <- strsplit(variables_select, "\\:" )  |> 
        unlist() |> 
        unique()
      
      variables_select <- unique(c(main_variables, variables_select))
    }
    
    coding <- rep(0, .self$length)
    positions <- which(.self$all_variables %in% variables_select)
    coding[positions] <- 1
    
    return(coding)
  } 
)

