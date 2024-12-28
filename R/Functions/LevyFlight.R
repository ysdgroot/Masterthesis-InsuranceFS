#' Create Levy flight sequence
#'
#' @param n number of values to be in the constructed
#' @param alpha index of the distribution. Special cases are 2=Gaussian, 1=Cauchy, 2/3 and 1/2. 
#' @param gamma scale unit of the process
#' @param stepsize stepsize to take for the levy flight
#'
#' @returns sequence of length n
#' @export
#' 
#' @references 
#' Mantegna, Rosario Nunzio. 
#' "Fast, accurate algorithm for numerical simulation of Levy stable stochastic processes." 
#' Physical Review E 49.5 (1994): 4677.
#' 
levyFlightProcess <- function(n, 
                              alpha, 
                              gamma=1, 
                              stepsize = 0.01){
  
  if(gamma < 1 | gamma > 2){
    stop("Gamma should be between 1 and 2 (both included)")
  }
  if(alpha < 0 | alpha > 2){
    stop("alpha should be between 0 and 2 (both included)")
  }
  
  # get value y, which is the denominator of S
  y <- rnorm(n, mean = 0, sd = 1)

  sigma_x <- ((gamma(1 + alpha) * sin(pi * alpha / 2))/
                 (alpha * gamma((alpha + 1)/2) * (2^((alpha-1)/2))) 
               )^(1/alpha)

  # get the value mu, which is the numerator of S
  x <- rnorm(n, 
             mean = 0, 
             sd = sqrt(sigma_x))

  # calculation of S
  step <- (x / (abs(y)^(1/alpha)))

  final <- stepsize * step * gamma^(1/alpha)
  
  return(final)
}

