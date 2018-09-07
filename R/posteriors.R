#' Pull posteriors from gRain results  
#'  
#' @description
#' Pull out a clean vector of posterior probabilities (useful for raster work)
#' 
#' 
#' @param value text
#'
#' @keywords text
#' @export
#' @examples
#' 

posteriors <- function(grain.results){
  
  results <- unlist(grain.results)[seq(2, 2 * length(grain.results)[1], 2)]
  return(results)
  
}
