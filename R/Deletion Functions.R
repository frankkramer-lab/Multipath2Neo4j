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

  if(is.null(result$error_code))
  {
    print(paste0("All nodes of label ", label," have been deleted successfully"))
  }

  return(result)
}


#' Delete a node  from a graph
#'
#' Delete a node or a set of nodes from a graph
#' @param connection The Neo4j connection object.
#' @param property_key The node property key name to be identified by.
#' @param property_key_value The node property key value to be found within.
#'
#' @return The result from the Neo4j call.
#' @export
#' @import neo4r
#' @export
#'
#' @examples
#' #To remove the node of name Protein1
#' removeNode(connection, "name", "Protein1")
removeNode <- function(connection, property_key, property_key_value){
  #If the function is called without passing a neo4j connection, raise an exception
  if(missing(connection)){
    stop("Neo4j Connection must be specified")
  }
  if(connection$ping() == FALSE){
    stop("Check your connection")
  }
  #If the function is called without passing the node property key, raise an exception
  if(missing(property_key) || is.null(property_key) || property_key == ""){
    stop("Node Property Key must be specified")
  }
  #If the function is called without passing the node property key value, raise an exception
  if(missing(property_key_value) || is.null(property_key_value) || property_key_value == ""){
    stop("Node Property Key value must be specified")
  }

  property_key_corrected = gsub(property_key, pattern = " ", replacement = "_")
  query = paste0("MATCH (n {", property_key_corrected, ": '")

  property_key_value_corrected = property_key_value
  if(grepl("'", property_key_value, fixed = TRUE))
  {
    property_key_value_corrected = unlist(strsplit(as.character(property_key_value), "'"))
    property_key_value_corrected = paste0(unlist(property_key_corrected), sep = "", collapse = "\\'")
  }

  query = paste0(query, property_key_value_corrected, "'}) DETACH DELETE n")

  result = call_neo4j(capture.output(cat(query)), connection, type = c("row", "graph"), output = c("r","json"), include_stats = FALSE, include_meta = FALSE)

  return(result)
}

