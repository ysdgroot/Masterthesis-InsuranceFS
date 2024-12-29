
#TODO: implement this function
getForm <- function(distMod, 
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
