# varList = list(varsGA)
# offset = NULL

#' Title
#'
#' @param distMod 
#' @param varList 
#' @param offset 
#'
#' @return
#' @export
getForm <- function(distMod, 
                    varList, 
                    offset = NULL){
  
  if(distMod$family[1] == 'poisson'){
    targetVar = 'claimNumber'
    linkFunc = distMod$link
  } else if (distMod$family[1] %in% c('Gamma', 'gaussian')){
    targetVar = 'claimSize'
    linkFunc = distMod$link
  } else if (distMod$family[1] == 'binomial'){
    targetVar = 'as.factor(claimSizeProb)'
    linkFunc = distMod$link
  }
  
  if(is.null(offset)){
    form <- paste(targetVar, '~ ', sep = "")
  } else {
    form <- paste(targetVar, '~ ', ' offset(', linkFunc, '(', offset, ')', ')', sep = "")
  }
  
  if(length(unlist(varList)) > 0){ 
    for(iList in 1:length(varList)){
      if(length(varList[[iList]] == 1)){
        form <- paste(form, ' + ', varList[[iList]], sep = "")
      } else {
        form <- paste(form, ' + ', paste(varList[[iList]], collapse = " + "), sep = "")
      }
    }
  } else { #only intercept + offset
    form <- paste(form, ' + 1', sep = "")
  }
  
  form <- as.formula(form)
  return(form)
}


#TODO: implement this function
getForm_v2 <- function(distMod, 
                       targetVar, 
                       varList,
                       offset = NULL){
  
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
}
