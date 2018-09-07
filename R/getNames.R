#' getNames for gRain
#' 
# Description
#'  
#' @description
#' More description 
#' 
#' 
#' @param value text
#'
#' @keywords text
#' @export
#' @examples
#' 
#
getNames <- function(prey){
  # get rownames of a state table defined by which species are present and which are not
  n <- length(prey)
  ch <- matrix("", 2^n, n)
  status <- rep("", 2^n)
  for(i in 0:((2^n) - 1)){
    x <- i
    for(j in 1:n){
      if(x%%2 == 0){
        ch[i+1, j] <- as.character(prey[j])
        x <- floor(x/2)
      } else {
        ch[i+1, j] <- "-"
        x <- floor(x/2)
      }
      status[i + 1] <- paste(status[i + 1], " ", ch[i + 1, j], sep='')
    }
  }
  return(status)
}