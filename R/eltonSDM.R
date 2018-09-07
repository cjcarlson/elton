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
#' r1 <- raster(matrix(nrow=5,ncol=5,c(rnorm(25,0.7,0.05))))
#' r2 <- raster(matrix(nrow=5,ncol=5,c(rnorm(25,0.7,0.05))))
#' r3 <- raster(matrix(nrow=5,ncol=5,c(rnorm(25,0.7,0.05))))
#' r4 <- raster(matrix(nrow=5,ncol=5,c(rnorm(25,0.7,0.05))))
#' r5 <- raster(matrix(nrow=5,ncol=5,c(rnorm(25,0.7,0.05))))
#' r6 <- raster(matrix(nrow=5,ncol=5,c(rnorm(25,0.7,0.05))))
#' r7 <- raster(matrix(nrow=5,ncol=5,c(rnorm(25,0.1,0.05))))
#' M2 <- matrix(c(rep(0,42),1,1,1,1,1,5,0), nrow=7, ncol=7)
#' rstack <- stack(r1,r2,r3,r4,r5,r6,r7)
#' posteriors <- eltonSDM(rstack,M2)
#' 

eltonSDM <- function(stack.prior, M, offset=NA, model.sel='ticks') {
  stack.post <- stack.prior
  
  for(i in 1:nrow(stack.prior)) {
    for(j in 1:ncol(stack.prior)) {
      if(sum(is.na(stack.prior[i,j]))>1) {
        print(((i-1)*ncol(stack.prior)+j)/(nrow(stack.prior[[1]])*ncol(stack.prior[[1]]))*100)
        
      } else {
        Pi <- as.vector(stack.prior[i,j])
        if(sum(is.na(offset))==1) {offset <- rep(0.1, dim(M)[1])}
        mytable <- BuildTable(M, Pi, offset, model=model.sel, alpha=1, beta=1)
        # look at print(mytable) to see how changing alpha and beta affect probabilities
        stack.post[i,j] <- posteriors(getMarginals_gRain(mytable,"tempfile.R"))
        print(((i-1)*ncol(stack.prior)+j)/(nrow(stack.prior[[1]])*ncol(stack.prior[[1]]))*100)
      }
    }
  }
  return(stack.post)
}

