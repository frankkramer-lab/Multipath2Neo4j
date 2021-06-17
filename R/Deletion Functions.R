#' Delete all nodes of the same label
#'
#' Delete all the nodes of the same label.
#' @param connection The Neo4j connection object.
#' @param label The nodes label to be deleted.
#'
#' @return The result from the Neo4j call.
#'
#' @export
#' @import neo4r
#' @export
#'
#' @examples
#' #To delete all labels
#' for(label in wntmully$layers$Name){
#'  deleteAllNodesOfLabel(connection,label)
#' }
#'
#' #To delete drugs nodes
#' deleteAllNodesOfLabel(connection,"drugs")
deleteAllNodesOfLabel <- function(connection, label){
  #If the function is called without passing a neo4j connection, raise an exception
  if(missing(connection)){
    stop("Neo4j Connection must be specified")
  }
  if(connection$ping() == FALSE){
    stop("Check your connection")
  }
  #If the function is called without passing a label, raise an exception
  if(missing(label) || is.null(label) || label == ""){
    stop("Label must be specified")
  }

  query = paste("MATCH (n:", label, sep="")
  query = paste(query, ") DETACH DELETE n", sep="")

  result = call_neo4j(query, connection, type = c("row", "graph"), output = c("r","json"), include_stats = FALSE, include_meta = FALSE)
  return(result)
}


