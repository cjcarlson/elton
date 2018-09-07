#' Run gRain and get all the results
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
 
getMarginals_gRain <- function(Table, file="test.R"){
  # automatically produce an R script file for running gRain
  write("require(gRain)", file=file)
  write("temp_fn <- function(){", file, append=TRUE)
  sp_levels <- "levels=c('absent', 'present'))"
  S <- length(Table$PiVector)
  for(i in 1:S){
    sp_name <- paste("V", i, sep='')
    if(dim(Table[[sp_name]])[1]==1){  # basal species
      X <- Table[[sp_name]]
      command <- paste("V", i, "<-cptable(~V", i, ", values=c(", 1 - X[1,1], ",", X[1,1], "), ", sp_levels, sep='')
      write(command, file=file, append=TRUE)
    } else {  # consumers 
      X <- Table[[sp_name]]
      nX <- dim(X)[1]
      command <- paste("V", i, "<-cptable(~V", i, "|", sep='')
      prey_str <- getPreys(abs(Table$M[, i]))
      command <- paste(command, prey_str, ", values=c(", sep='')
      for(j in 1:(nX - 1)){
        command <- paste(command, 1 - X[nX - j + 1, 1], ",", X[nX - j + 1, 1], ",", sep='')  # l-to-r: all prey present to all extinct
      }
      command <- paste(command, 1 - X[1, 1], ",", X[1, 1], "), ", sp_levels, sep='')
      write(command, file, append=TRUE)
    }
  }
  command2 <- paste("plist<-compileCPT(list(", sep='')
  for(i in 1:(S-1)){
    command2 <- paste(command2, "V", i, ",", sep='')
  }
  command2 <- paste(command2, "V", S, "))", sep='')
  write(command2, file, append=TRUE)
  write("BN<-grain(plist)", file, append=TRUE);
  write("Results_tmp<-querygrain(BN)", file, append=TRUE)
  write("return(Results_tmp)", file, append=TRUE)
  write("}", file, append=TRUE)
  
  # Run the script
  source(file)
  r <- temp_fn()
  results <- list()
  # Re-orgainse the sequence in the output
  for(i in 1:S){
    sp_name <- paste("V", i, sep='')
    results[[sp_name]] <- r[[sp_name]]
  }
  return(results)	
}