#' Build Table
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
#'

BuildTable <- function(M, Pi, offset, model, alpha=1, beta=1){
  # form input for getMarginals_gRain()  including state tables for each species
  use_sf <- 1  # USER-DEFINED OPTION
  if (use_sf==1){significant_figures <- 7}
  PiVector <- Pi
  S <- length(PiVector)
  Table <- list()
  Table$PiVector <- PiVector
  Table$M <- M
  for (i in 1:S){
    variable_name <- paste("V", i, sep='')
    prey <- which(M[, i] != 0)
    n <- length(prey)
    if (n==0){
      # species is basal
      X <- matrix(0, 1, 1)
      if (use_sf==1){X[1, 1] <- signif(PiVector[i], digits=significant_figures)}
      else{X[1, 1] <- PiVector[i]}
      Table[[variable_name]] <- X
    }
    else{
      # species is a consumer
      X <- matrix(0, 2^sum(M[, i] != 0), 1)
      rownames(X) <- getNames(prey)  # top-to-bottom: all influencing species absent to all present
      if(model=="linear" || model=="nonlinear"){
        for(j in 1:(2^n)){
          tmp <- j - 1
          v <- numeric(n)  # create a vector of length n (entry-1 if species present in state)
          for(k in 1:n){
            if((tmp %% 2) == 0){
              v[k] = 0
              tmp <- floor(tmp/2)
            } else {
              v[k] = 1
              tmp <- floor(tmp/2)
            }
          }
          # Fraction resource lost; works for binary and flow
          frac_loss <- 1 - (M[M[, i]!=0, i] %*% v / (M[M[, i]!=0, i] %*% rep(1, n)))
          if(model=="linear"){
            if (use_sf==1){X[j, 1] <- signif(GetLinearResponse(frac_loss, PiVector[i]), digits=significant_figures)}
            else{X[j, 1] <- GetLinearResponse(frac_loss, PiVector[i])}
          }
          if(model=="nonlinear"){
            if(use_sf==1){X[j, 1] <- signif(GetNonLinearResponse(frac_loss, PiVector[i], alpha, beta), digits=significant_figures)}
            else{X[j, 1] <- GetNonLinearResponse(frac_loss, PiVector[i], alpha, beta)}
          }
        }
      }
      else if(model=="topo"){
        for(j in 1:(2^n)){
          tmp <- j-1
          v <- numeric(n)
          X[j, 1] <- 1
          for(k in 1:n){
            if(tmp%%2==0){
              v[k] = 0
              tmp <- floor(tmp/2)
            } else{
              v[k] = 1
              X[j, 1] <- PiVector[i]
              break
            }
          }
        }
      }
      else if(model == "binaryplusminus_linear"){
        posEdges <- sum(M[,i] > 0)  # Total number of facilitatory dependencies for i
        negEdges <- sum(M[,i] < 0)  # Total number of inhibitory dependencies for i
        maxunits <- max(posEdges, negEdges)
        for(j in 1:(2^n)){
          tmp <- j - 1
          v <- numeric(n)  # create a vector of length n
          ## Determine which resources to consider for case j
          for(k in 1:n){
            if((tmp %% 2) == 0){
              v[k] = 1
              tmp <- floor(tmp/2)
            } else {
              v[k] = 0
              tmp <- floor(tmp/2)
            }
          }
          netcontributingunits <- M[M[, i] != 0, i]%*%v  # Total number of facilitatory minus inhibitory dependencies considered in case j
          if(use_sf==1){
            X[j, 1] <- signif(GetBinaryLinearFacInhResponse(netcontributingunits, maxunits, PiVector[i]), digits=significant_figures)
          }
          else{
            X[j, 1] <- GetBinaryLinearFacInhResponse(netcontributingunits, maxunits, PiVector[i])
          }
        }
      }
      else if(model == "binaryplusminus_OR"){
        posEdges <- sum(M[,i] > 0)  # Total number of facilitatory dependencies for i
        negEdges <- sum(M[,i] < 0)  # Total number of inhibitory dependencies for i
        maxunits <- max(posEdges, negEdges)
        for(j in 1:(2^n)){
          tmp <- j - 1
          v <- numeric(n)  # create a vector of length n
          ## Determine which influencing species to consider for case j
          for(k in 1:n){
            if((tmp %% 2) == 0){
              v[k] = 1
              tmp <- floor(tmp/2)
            } else {
              v[k] = 0
              tmp <- floor(tmp/2)
            }
          }
          netcontributingunits <- M[M[, i] != 0, i]%*%v  # Total number of facilitatory minus dependencies considered in case j
          if(use_sf==1){
            X[j, 1] <- signif(GetBinaryORFacInhResponse(netcontributingunits, maxunits, PiVector[i]), digits=significant_figures)
          }
          else{
            X[j, 1] <- GetBinaryORFacInhResponse(netcontributingunits, maxunits, PiVector[i])
          }
        }
      }
      else if(model == "binaryplusminus_nonlinear"){
        posEdges <- sum(M[,i] > 0)  # Total number of facilitatory dependencies for i
        negEdges <- sum(M[,i] < 0)  # Total number of inhibitory dependencies for i
        maxunits <- max(posEdges, negEdges)
        for(j in 1:(2^n)){
          tmp <- j - 1
          v <- numeric(n)  # create a vector of length n
          ## Determine which dependencies to consider for case j
          for(k in 1:n){
            if((tmp %% 2) == 0){
              v[k] = 1
              tmp <- floor(tmp/2)
            } else {
              v[k] = 0
              tmp <- floor(tmp/2)
            }
          }
          netcontributingunits <- M[M[, i] != 0, i]%*%v  # Total number of facilitatory minus inhibitory dependencies considered in case j
          tempvector <- M[M[, i] != 0, i]
          tempvector[tempvector < 1] <- 0
          posEdges_state <- tempvector%*%v
          tempvector <- M[M[, i] != 0, i]
          tempvector[tempvector > -1] <- 0
          negEdges_state <- abs(tempvector%*%v)
          if(use_sf==1){
            X[j, 1] <- signif(GetBinaryNonLinearFacInhResponse(netcontributingunits, posEdges_state, negEdges_state, maxunits, PiVector[i], alpha, beta), digits=significant_figures)
          }
          else{
            X[j, 1] <- GetBinaryNonLinearFacInhResponse(netcontributingunits, posEdges_state, negEdges_state, maxunits, PiVector[i], alpha, beta)
          }
        }
      }
      else if(model == "ticks"){
        posEdges <- sum(M[,i])  # Total number of facilitatory dependencies for i
        negEdges <- 0  # Total number of inhibitory dependencies for i
        maxunits <- max(posEdges, negEdges)
        for(j in 1:(2^n)){
          tmp <- j - 1
          v <- numeric(n)  # create a vector of length n
          ## Determine which dependencies to consider for case j
          for(k in 1:n){
            if((tmp %% 2) == 0){
              v[k] = 1
              tmp <- floor(tmp/2)
            } else {
              v[k] = 0
              tmp <- floor(tmp/2)
            }
          }
          netcontributingunits <- M[M[, i] != 0, i]%*%v  # Total number of facilitatory minus inhibitory dependencies considered in case j
          tempvector <- M[M[, i] != 0, i]
          tempvector[tempvector < 1] <- 0
          posEdges_state <- tempvector%*%v
          tempvector <- M[M[, i] != 0, i]
          tempvector[tempvector > -1] <- 0
          negEdges_state <- abs(tempvector%*%v)
          if(use_sf==1){
            X[j, 1] <- signif(TicksResponse(netcontributingunits, posEdges_state, negEdges_state, maxunits, PiVector[i], offset[i], alpha, beta), digits=significant_figures)
          }
          else{
            X[j, 1] <- TicksResponse(netcontributingunits, posEdges_state, negEdges_state, maxunits, PiVector[i], offset[i], alpha, beta)
          }
        }
      }
      else {
        stop("You must specify a response form: binaryplusminus_linear")
      }
      Table[[variable_name]] <- X
    }
  }
  Table$model <- model
  return(Table)
}
