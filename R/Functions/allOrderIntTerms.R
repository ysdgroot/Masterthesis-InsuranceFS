#TODO: documentation of the function
allOrderIntTerms <- function(variableNames, order = 2){
  # check on the variable m
  if(m < 1 | m%%1 != 0){
    stop("value of m should be equal or higher than 1 and a whole number")
  }
  
  # get all the combinations for each order
  full_list <- c()
  for (m in 2:order){
    combinations <- combn(vars, m = m, simplify = FALSE) |> 
      lapply(FUN = \(x)(paste(x, collapse = "*"))) |> 
      unlist()
    
    full_list <- c(full_list, combinations)
  }
  return(full_list)
}



allFirstOrderIntTerms <- function(variableNames){
  count <- 1
  if(length(variableNames) > 1){
    firstOrderTerms <- rep(NA, ((length(variableNames) - 1) * length(variableNames))/2)
    for(varLeft in 1:(length(variableNames) - 1)){
      for(varRight in (varLeft + 1):length(variableNames)){
        firstOrderTerms[count] <- paste(variableNames[varLeft], variableNames[varRight], sep = '*')
        count <- count + 1
      }
    }
    return(firstOrderTerms)
  }  
  return(NULL)
}
