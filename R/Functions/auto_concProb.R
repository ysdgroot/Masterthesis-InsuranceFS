#' Title
#'
#' @param inputDT 
#' @param type 
#' @param nu 
#'
#' @return
#' @export
#'
#' @examples
auto_concProb <- function(inputDT, 
                          type = 'bin',
                          nu = 0){
  if(sum(names(inputDT) == 'observed')) setnames(inputDT, 'observed', 'obs')
  if(sum(names(inputDT) == 'predicted')) setnames(inputDT, 'predicted', 'pred')
  if(type == 'cont'){
    res <- concProb_cont_fast(inputDT$obs, inputDT$pred, nu = nu)$concProb
  } else if(type == 'bin'){
    res <- concProb_bin_fast(inputDT$obs, inputDT$pred)$concProb
  }
  return(res)
}