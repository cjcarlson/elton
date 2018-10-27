
#' GetBinaryLinearFacInhResponse
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

################ RESPONSE TO THE PRESENCE OF OTHER SPECIES
GetBinaryLinearFacInhResponse <- function(netcontributingunits, maxunits, p){
  # linear response relating number of faciliatory and/or inhibitory species present to probability of focal species presence
  if(p > 0.5){
    return(p + netcontributingunits * (1 - p) / maxunits)
  }
  else {
    return(p + netcontributingunits * p / maxunits)
  }
}


#' GetBinaryORFacInhResponse 
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

GetBinaryORFacInhResponse <- function(netcontributingunits, maxunits, p){
  # OR response relating number of faciliatory and/or inhibitory species present to probability of focal species presence
  if(netcontributingunits > 0){
    stepfactor <- 1
  }
  else if(netcontributingunits < 0){
    stepfactor <- -1
  }
  else{
    stepfactor <- 0
  }
  if(p > 0.5){
    return(p + stepfactor * (1 - p))
  }
  else {
    return(p + stepfactor * p)
  }
}

#' GetBinaryNonLinearFacInhResponse
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


GetBinaryNonLinearFacInhResponse <- function(netcontributingunits, posEdges, negEdges, maxunits, p, a, b){
  # nonlinear response (beta function with parameters a and b) relating number of faciliatory and/or inhibitory species present to probability of focal species presence
  if(p > 0.5){
    positivecontribution <- (1 - p) * (pbeta(posEdges / maxunits, a, b))
    negativecontribution <- (1 - p) * (pbeta(negEdges / maxunits, a, b))
    return(p + positivecontribution - negativecontribution)
  }
  else {
    positivecontribution <- p * (pbeta(posEdges / maxunits, a, b))
    negativecontribution <- p * (pbeta(negEdges / maxunits, a, b))
    return(p + positivecontribution - negativecontribution)
  }
}


#' TickResponse
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

TicksResponse <- function(netcontributingunits, posEdges, negEdges, maxunits, p, offset, a, b){
  # nonlinear response (beta function with parameters a and b) relating number of faciliatory and/or inhibitory species present to probability of focal species presence
  if(p > 0.5){
    if(posEdges==(maxunits/2)){
      returnvalue <- p
    }
    else if (posEdges > (maxunits/2)){
      positivecontribution <- (p - offset) * pbeta((posEdges - (maxunits/2))/(maxunits/2), a, b)
      if((p + positivecontribution) >= 1){
        returnvalue <- 1
      }
      else {
        returnvalue <- p + positivecontribution
      }
    }
    else if (posEdges < (maxunits/2)){
      positivecontribution <- (p - offset) * pbeta(((maxunits/2) - posEdges)/(maxunits/2), a, b)
      returnvalue <- p - positivecontribution
      
    }
    else {print("ERROR in TicksResponse()")}
    positivecontribution <- p * (pbeta(posEdges / maxunits, a, b))
    return(returnvalue)
  }
  else {
    if(posEdges==(maxunits/2)){
      returnvalue <- p
    }
    else if (posEdges > (maxunits/2)){
      positivecontribution <- (p - offset) * pbeta((posEdges - (maxunits/2))/(maxunits/2), a, b)
      returnvalue <- p + positivecontribution
    }
    else if (posEdges < (maxunits/2)){
      positivecontribution <- (p - offset) * pbeta(((maxunits/2) - posEdges)/(maxunits/2), a, b)
      returnvalue <- p - positivecontribution
      
    }
    else {print("ERROR in TicksResponse()")}
    positivecontribution <- p * (pbeta(posEdges / maxunits, a, b))
    return(returnvalue)
  }
}


TicksResponse2 <- function(posEdges, p, maxunits, a, b, offset=0){
  # nonlinear response (beta function with parameters a and b) relating number of faciliatory and/or inhibitory species present to probability of focal species presence
  if(p > 0.5){
    if(posEdges==(maxunits/2)){
      returnvalue <- p
    }
    else if (posEdges > (maxunits/2)){
      positivecontribution <- (p - offset) * pbeta((posEdges - (maxunits/2))/(maxunits/2), a, b)
      if((p + positivecontribution) >= 1){
        returnvalue <- 1
      }
      else {
        returnvalue <- p + positivecontribution
      }
    }
    else if (posEdges < (maxunits/2)){
      positivecontribution <- (p - offset) * pbeta(((maxunits/2) - posEdges)/(maxunits/2), a, b)
      returnvalue <- p - positivecontribution
      
    }
    else {print("ERROR in TicksResponse()")}
    positivecontribution <- p * (pbeta(posEdges / maxunits, a, b))
    return(returnvalue)
  }
  else {
    if(posEdges==(maxunits/2)){
      returnvalue <- p
    }
    else if (posEdges > (maxunits/2)){
      positivecontribution <- (p - offset) * pbeta((posEdges - (maxunits/2))/(maxunits/2), a, b)
      returnvalue <- p + positivecontribution
    }
    else if (posEdges < (maxunits/2)){
      positivecontribution <- (p - offset) * pbeta(((maxunits/2) - posEdges)/(maxunits/2), a, b)
      returnvalue <- p - positivecontribution
      
    }
    else {print("ERROR in TicksResponse()")}
    positivecontribution <- p * (pbeta(posEdges / maxunits, a, b))
    return(returnvalue)
  }
  return(returnvalue)
}
