# Importing library and Functions --------------------------------------------

source(file.path("R", "0-Packages.R"))
# source all the functions in the folder Functions
sapply(list.files(file.path("R", "Functions"), 
                  pattern = "*.R", 
                  full.names = TRUE, 
                  recursive = TRUE), 
       FUN = "source", 
       echo = FALSE, 
       prompt.echo = FALSE)

# TransferFunctions -------------------------------------------------------

baseClassTransferFunctions <- list(S1 = TransferFunction$new("S1", 
                                                             fun = function(x){sigmoid(x)}, 
                                                             type = "S"),
                                   S2 = TransferFunction$new("S2", 
                                                             fun = function(x){sigmoid(x/2)}, 
                                                             type = "S"), 
                                   S3 = TransferFunction$new("S3", 
                                                             fun = function(x){sigmoid(2*x)}, 
                                                             type = "S"), 
                                   V1 = TransferFunction$new("V1", 
                                                             fun = function(x){abs(x)/(sqrt(1 + x**2))}, 
                                                             type = "V"), 
                                   V2 = TransferFunction$new("V2", 
                                                             fun = function(x){abs(tanh(x))}, 
                                                             type = "V"), 
                                   V3 = TransferFunction$new("V3", 
                                                             fun = function(x){abs(2*atan(pi * x /2)/pi)}, 
                                                             type = "V"))

