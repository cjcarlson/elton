#' Is it a DAG?
#' 
# We gotta know 
#'  
#' @description
#' Check if a graph is directed and acyclic, converting the interaction matrix into a positive-only format that can be checked by igraph::is.dag
#' 
#' 
#' @param myDAG your input graph, in the form of a matrix with values -1, 0, or +1
#'
#' @keywords directed acyclic graph
#' @export
#' @examples

isDAG <- function(myDAG){ 
  M <- myDAG 
  M[is.na(M)] <- 0 
  M[M == -1] = 1 
  graph <- graph.adjacency(M, mode="directed",) 
  return(is.dag(graph))
}
