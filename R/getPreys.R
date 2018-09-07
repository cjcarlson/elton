#' Get Preys
#' 
# Description
#'  
#' @description
#' Pull dependent species
#' 
#' 
#' @param value text
#'
#' @keywords text
#' @export
#' @examples
#' 
#' 

getPreys <- function(v){
  # get string of names of dependencies of a species with "+" in between
  n <- length(v)
  p <- sum(v > 0)
  prey_str <- ''
  for(i in 1:n){
    if(p > 1){
      if(v[i] > 0){
        prey_str <- paste(prey_str, 'V', i, "+", sep='')
        p <- p - 1
      }
    } else{
      break
    }	
  }
  for(j in i:n){
    if(v[j] > 0){
      prey_str <- paste(prey_str, 'V', j, sep='')
    }
  }
  return(prey_str)
}
