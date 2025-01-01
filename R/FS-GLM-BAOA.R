# Importing library -------------------------------------------------------

source(file.path("R", "Packages.R"))
source(file.path("R", "General Parameters.R"))

# source all the functions for the GA
sapply(list.files(file.path("R", "BAOA"), 
                  pattern = "*.R", 
                  full.names = TRUE), 
       FUN = "source", 
       echo = FALSE, 
       prompt.echo = FALSE)


# Model run ---------------------------------------------------------------

## Parameters --------------------------------------------------------------

mfitness <- memoise::memoise(concProb_glm_bin)

# random suggestion sample 
suggestions <- matrix(as.double(NA), 
                      nrow = 10, 
                      ncol = VH$getLength())
for(j in 1:10) { positions <- sample.int(n = VH$getLength(), 
                                         size = 20)
suggestion <- rep(0,  VH$getLength())
suggestion[positions] <- 1

suggestions[j,] <- suggestion
}

suggestions <- rbind(VH$getCoding(vars), 
                     suggestions)

## Running function --------------------------------------------------------

#TODO: create function to run using the BAOA algo 



