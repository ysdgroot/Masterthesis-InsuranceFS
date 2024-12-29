library(R6)

#' R6 Class Representing variable handler
#' This to be able to convert Binary into variables and vice versa.
#' Also be able to construct a formula which can be used like a GLM
#'
#' @description
#' VariableHandler has the main variables and an order
#' Order is the degree in interaction terms. 
#' Order equals 1, means no interactions, 
#' 2: interactions of first order (2 variables)
#' 3: interactions of second order (3 variables)
#' etc.
#' 
#' @details
#' The variable handles will create all the different interactions based on order
#' 
#' The variable handler can also work by fixing or ignoring variables. 
#' All the 'fixed' or 'ignored' variables are not used for the coding (binary vector).
#' Only the fixed are used for creating the formula. 
#' The ignored will never be used, not in the coding nor the formula.
#' This can be useful when knowing a interaction or variable should not be used for the construction
#' 

R6::R6Class("VariableHandler", 
             public = list(
               initialize = function(variables, 
                                     order = 1){
                 # set default values
                 private$main_variables <- sort(variables)
                 private$order <- order 
                 
                 # check on the variable order
                 if(order < 1 | order%%1 != 0){
                   stop("value of m should be equal or higher than 2 and a whole number")
                 }
                 
                 # get all the combinations for each order
                 full_list <- c()
                 for (m in 1:order){
                   combinations <- combn(sort(variables), m = m, simplify = FALSE) |> 
                     lapply(FUN = \(x)(paste(x, collapse = ":"))) |> 
                     unlist()
                   
                   full_list <- c(full_list, combinations)
                 }
                 
                 private$all_variables <- full_list
                 private$used_variables <- full_list
                 private$ignored <- character(0)
                 private$fixed <- character(0)
               }, 
#' @returns the number binary positions needed
               getLength = function(){
                 return(length(private$used_variables))
               }, 

#' @description
#' get some basic info about which variables are used, fixed, ignored. 
#' @returns list with 7 elements
#' nUsed = number of used variables (same as `getLength()`)
#' nFixed = number variables which are fixed 
#' nIgnored = number of variables which are ignored
#' AllVariables = all possible variables
#' Used = all variables which are used for the binary construction 
#' Fixed = all variables which are fixed
#' Ignored = all variables which are ignored
               getInfo = function(){
                 return(list("nUsed" = length(private$used_variables), 
                             "nFixed" =length(private$fixed), 
                             "nIgnored" = length(private$ignored), 
                             "AllVariables" = private$all_variables, 
                             "Used" = self$getUsedVariables(), 
                             "Fixed" = self$getFixedVariables(), 
                             "Ignored" = self$getIgnoredVariables()))
               },
#' @description
#' set list of variables which needs to be fixed
#' @param variables character (vector), only values from `getAllVariables()` should be used
#'
#' @returns NULL
               setFixedVar = function(variables){
                 private$setterList(variables = variables, 
                                    type = "fixed")
               }, 
#' @description
#' set list of variables which needs to be ignored
#' @param variables character (vector), only values from `getAllVariables()` should be used
#'
#' @returns NULL
               setIgnoreVar = function(variables){
                 private$setterList(variables = variables, 
                                    type = "ignored")
               }, 

#' @param variables character (vector), only values from `getAllVariables()` should be used. 
#' values should be in `getFixedVariables()`, otherwise an error will be thrown
#'
#' @returns NULL
               removeFixedVar = function(variables){
                 # test if variable is already fixed
                 if (!all(variables %in% private$fixed)){
                   stop(sprintf("The following variables were not in the fixed list: %s", 
                                paste(setdiff(variables, private$fixed), 
                                      collapse = ", ")))
                 }
                 
                 private$fixed <- setdiff(private$fixed,variables)
                 private$used_variables <- unique(union(private$used_variables, variables))
                 
               },
#' @param variables character (vector), only values from `getAllVariables()` should be used. 
#' values should be in `getIgnoredVariables()`, otherwise an error will be thrown
#'
#' @returns NULL
               removeIgnoreVar = function(variables){
                 # test if variable is already ignore
                 if (!all(variables %in% private$ignored)){
                   stop(sprintf("The following variables were not in the ignore list: %s", 
                                paste(setdiff(variables, private$ignored), 
                                      collapse = ", ")))
                 }
                 
                 private$ignored <- setdiff(private$ignored,variables)
                 private$used_variables <- unique(union(private$used_variables, variables))
               },
#' @description
#' set list of variables which needs to be added to the ignored
#' @param variables character (vector), only values from `getAllVariables()` should be used
#'
#' @returns NULL
               addIgnoreVar = function(variables){
                 self$setIgnoreVar(unique(c(private$ignored,variables)))
               }, 
#' @description
#' set list of variables which needs to be added to the fixed
#' @param variables character (vector), only values from `getAllVariables()` should be used
#'
#' @returns NULL
               addFixedVar = function(variables){
                 self$setFixedVar(unique(c(private$fixed,variables)))
               }, 
#' @description
#' put back values to be used for the coding and/or formula 
#'
#' @param variables character (vector), only values from `getAllVariables()` should be used. 
#' variables should be in `getIgnoredVariables()` or `getFixedVariables()`. 
#' All useful variables will be removed from the ignored/fixed list and put back in the used variables list
#' 
#' @param message logical, if message should be shown from where the values is taken from. 
#' @returns
               addUsedVar = function(variables, 
                                     message = TRUE){
                 #test if variables can be found in all variables
                 if (!all(variables %in% private$all_variables)){
                   stop(sprintf("The following variables are not found: %s", 
                                paste(setdiff(variables, private$all_variables), 
                                      collapse = ", ")))
                 }
                 
                 inter_ignore <- intersect(variables, private$ignored)
                 if (any(variables %in% private$ignored)){
                   if(message){
                     message(sprintf("The following variables were in the ignored list: %s", 
                                     paste(inter_ignore, 
                                           collapse = ", ")))
                   }
                   self$removeIgnoreVar(inter_ignore)
                 }
                 
                 inter_fixed <- intersect(variables, private$fixed)
                 if (any(variables %in% self$fixed)){
                   if(message){
                     message(sprintf("The following variables were in the fixed list: %s", 
                                     paste(inter_fixed, 
                                           collapse = ", ")))
                   }
                   self$removeFixedVar(inter_fixed)
                 }
                 
                 private$used_variables <- unique(union(union(private$used_variables, inter_ignore), inter_fixed))
                 
               },
#' @description
#' get the used variables based on the binary code given
#' @param coding binary vector of length `getLength()`
#' @param withMain logical, if the main-variables (only for interations) should be included or not
#' Be aware it will only look at the variables for the coding. 
#' So if a main variable is in the fixed or ignored list, it will not be added to the coding. 
#' @param message logical, is message if a main variable is not used or not. 
#' Only when `withMain` is set to `TRUE`
#'
#' @returns list of variables based on coding. 
#' The length will be the sum of the coding
               getVariables = function(coding, 
                                       withMain = FALSE, 
                                       message = FALSE){
                 
                 if(length(coding) != self$getLength()){
                   stop(sprintf("Length of the coding is incorrect, it should have length %d", 
                                self$getLength()))
                 }
                 
                 if(!all(coding %in% c(0,1))){
                   stop("It should only contain 0 and 1")
                 }
                 
                 selected_coding <- coding
                 if (withMain){
                   # update the coding based on the function
                   selected_coding <- self$getCoding(variables = self$getUsedVariables()[coding == 1], 
                                                      withMain = TRUE, 
                                                      message = message)
                 }
                 
                 return(self$getUsedVariables()[selected_coding == 1])
               }, 
#' @description
#' Get the binary coding based on the variables given. 
#'
#' @param variables character (vector), only values from `getUsedVariables()` should be used, 
#' if not, a warning will be given that those variables are not used
#' @param withMain logical, if the main-variables (only for interations) should be included or not
#' Be aware it will only look at the variables for the coding. 
#' So if a main variable is in the fixed or ignored list, it will not be added to the coding.
#' @param message logical, is message if a main variable is not used or not. 
#' Only when `withMain` is set to `TRUE`
#'
#' @returns binary vector of length `getLength()`
#' The number of 1 is the number of variables given minus those that are ignored based on the warning given.
               getCoding = function(variables, 
                                    withMain = FALSE, 
                                    message = TRUE){
                 
                 if(!all(variables %in% private$used_variables)){
                   setdiff_variables <- setdiff(variables,private$used_variables)
                   warning(sprintf("The following variables are not in the coding %s", 
                                   paste(setdiff_variables, 
                                         collapse = ", ")))
                 }
                 used_variable_coding <-  intersect(private$used_variables, variables)
                 
                 if(withMain){
                   used_main_variables <- strsplit(used_variable_coding, "\\:" )  |> 
                     unlist() |> 
                     unique()
                   
                   used_variable_coding <- unique(c(used_main_variables, used_variable_coding))
                   
                   # give warning that for some the coding of the main variables are not included
                   if(!all(used_main_variables %in% private$used_variables)){
                     setdiff_main <- setdiff(used_main_variables,private$used_variables)
                     if(message){
                       warning(sprintf("The following (main) variables are not in the coding %s", 
                                       paste(setdiff_main, 
                                             collapse = ", ")))
                     }
                   }
                 }
                 
                 coding <- rep(0, self$getLength())
                 positions <- which(self$getUsedVariables() %in% used_variable_coding)
                 coding[positions] <- 1
                 
                 return(coding)
               },
#' @description
#' Construction of a formula based on the coding given and the fixed variables
#' 
#' @param coding binary vector of length `getLength()`
#' @param distMod family of distribution with a link function. 
#' Only the link function will be used. 
#' @param targetVar character, variable name to be put before the `~`
#' @param offset NULL or character, if NULL then ignored, 
#' if not it will be transformed by the link function and put as offset
#' @param withMain logical, if the main-variables (only for interations) should be included or not
#' Be aware it will only look at the variables for the coding. 
#' So if a main variable is in the fixed or ignored list, it will not be added to the coding. 
#' @param message logical, is message if a main variable is not used or not. 
#' Only when `withMain` is set to `TRUE`
#'
#' @returns formula based on fixed variables and the coding given
               getFormula = function(coding, 
                                     distMod,
                                     targetVar, 
                                     offset = NULL, 
                                     withMain = FALSE, 
                                     message = FALSE){
                 
                 varList <- c(self$getVariables(coding, 
                                                 withMain = withMain,
                                                 message = message), 
                              private$fixed)
                 
                 linkFunc <- distMod$link
                 form <- paste0(targetVar, "~ ")
                 
                 if (!is.null(offset)){
                   form <- paste0(form, sprintf("offset(%s(%s)) + ",linkFunc, offset))
                 }
                 
                 if (length(unlist(varList)) > 0 ){
                   form <- paste0(form, paste0(unlist(varList), collapse = "+"))
                 } else {
                   form <- paste0(form, "1")
                 }
                 
                 form <- as.formula(form)
                 return(form)
               }, 

#' @returns list with all the possible variables
               getAllVariables = function(){return(private$all_variables)}, 
#' @returns list with all the main variables
               getMainVariables = function(){return(private$main_variables)}, 
#' @returns list with all the variables used for the coding
               getUsedVariables = function(){return(private$all_variables[private$all_variables %in% private$used_variables])}, 
#' @returns list with all the variables which should be included in the formula
               getFixedVariables = function(){return(private$all_variables[private$all_variables %in% private$fixed])}, 
#' @returns list with all the variables that needs to be ignored
               getIgnoredVariables = function(){return(private$all_variables[private$all_variables %in% private$ignored])}
             ), 
             private = list(main_variables = NULL, 
                            order = NULL, 
                            all_variables = NULL, 
                            fixed = NULL,
                            ignored = NULL,
                            used_variables = NULL, 
                            setterList = function(variables,
                                                  type){
                              
                              ## CHECKS
                              #test if variables can be found in all variables
                              if (!all(variables %in% private$all_variables)){
                                stop(sprintf("The following variables are not found: %s", 
                                             paste(setdiff(variables, private$all_variables), 
                                                   collapse = ", ")))
                              }
                              
                              if(type == "fixed"){
                                message <- "The following variables are already in the ignored list: %s"
                                list_check <- private$ignored
                                }
                              else if(type == "ignored"){
                                message <- "The following variables are already in the fixed list: %s"
                                list_check <- private$fixed
                                }
                              else{stop("Incorrect type")}
                                                   
                              # test if variable is already fixed
                              if (any(variables %in% list_check)){
                                stop(sprintf(message, 
                                             paste(intersect(variables, list_check), 
                                                   collapse = ", ")))
                              }
                              ## END CHECKS
                              
                              if(type == "fixed"){
                                # put back all the fixed values 
                                private$used_variables <- unique(c(private$used_variables, 
                                                                   private$fixed))
                                #set the fixed values 
                                private$fixed <- variables
                                
                              } else {
                                # put back all the ignored values 
                                private$used_variables <- unique(c(private$used_variables, 
                                                                   private$ignored))
                                #set the ignored values 
                                private$ignored <- variables
                              }
                        
                              #remove from the used variables 
                              private$used_variables <- setdiff(private$used_variables, 
                                                                variables)
                            })) -> VariableHandler
