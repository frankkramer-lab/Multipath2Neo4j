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


#' Delete a node in the Neo4j graph
#'
#' Delete a node or a set of nodes from a graph
#' @param connection The Neo4j connection object.
#' @param property_key The node property key name to be identified by.
#' @param property_key_value The node property key value to be found within.
#'
#' @return The result from the Neo4j call.
#'
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


#' Delete a relationship between 2 nodes in the Neo4j graph
#'
#' Delete a relationship between 2 nodes in the Neo4j graph. If no relationship property keys are given as argument, all relationships between the 2 nodes are deleted.
#' @param connection The Neo4j connection object.
#' @param started_node_property The started node property key name to be identified by.
#' @param started_node_property_value The started node property key value to be found within.
#' @param end_node_property The end node property key name to be identified by.
#' @param end_node_property_value The end node property key value to be found within.
#' @param relationship_property_keys The relationship property keys, in the form of dataframe.
#'
#' @return The result from the Neo4j call.
#' @export
#' @import neo4r
#' @export
#'
#' @examples
#' #Get the dataframe of relationships to be created
#' relationships = prepareRelationshipsDataframeToCreate(wntmully,nodes)
#'
#' #Delete all relationships in the Neo4j database
#' for(row in 1:nrow(relationships)){
#'   removeRelationship(connection, "name", relationships[row,'V1'], "name", relationships[row,'V2'])
#' }
#'
#' #Delete the relationships started from the node of name Complex10 and ended in the node of name Protein1 in the Neo4j database
#' removeRelationship(connection, "name", "Complex10", "name", "Protein1")

removeRelationship <- function(connection, started_node_property,
                       started_node_property_value, end_node_property,
                       end_node_property_value, relationship_property_keys = NULL){
  #If the function is called without passing a neo4j connection, raise an exception
  if(missing(connection)){
    stop("Neo4j Connection must be specified")
  }
  if(connection$ping() == FALSE){
    stop("Check your connection")
  }

  #If the function is called without passing the started node property key, raise an exception
  if(missing(started_node_property) || is.null(started_node_property) || started_node_property == ""){
    stop("Started Node Property Key must be specified")
  }
  #If the function is called without passing the started node property key value, raise an exception
  if(missing(started_node_property_value) || is.null(started_node_property_value) || started_node_property_value == ""){
    stop("Started Node Property Key Value must be specified")
  }

  #If the function is called without passing the end node property key, raise an exception
  if(missing(end_node_property) || is.null(end_node_property) || end_node_property == ""){
    stop("End Node Property Key must be specified")
  }
  #If the function is called without passing the end node property key value, raise an exception
  if(missing(end_node_property_value) || is.null(end_node_property_value) || end_node_property_value == ""){
    stop("End Node Property Key Value must be specified")
  }

  started_node_property_corrected = gsub(started_node_property, pattern = " ", replacement = "_")
  end_node_property_corrected = gsub(end_node_property, pattern = " ", replacement = "_")

  started_node_property_value_corrected = started_node_property_value
  if(grepl("'", started_node_property_value, fixed = TRUE))
  {
    started_node_property_value_corrected = unlist(strsplit(as.character(started_node_property_value), "'"))
    started_node_property_value_corrected = paste0(unlist(started_node_property_value_corrected), sep = "", collapse = "\\'")
  }

  end_node_property_value_corrected = end_node_property_value
  if(grepl("'", end_node_property_value, fixed = TRUE))
  {
    end_node_property_value_corrected = unlist(strsplit(as.character(end_node_property_value), "'"))
    end_node_property_value_corrected = paste0(unlist(end_node_property_value_corrected), sep = "", collapse = "\\'")
  }

  query = paste0("MATCH (n {", started_node_property_corrected, ": '", started_node_property_value_corrected, "'})-[r")

  if(!is.null(relationship_property_keys)){
    query = paste0(query, "{ ")
    for(property in colnames(relationship_property_keys)){
      query = paste0(query, property,  ": '")

      property_key_corrected = relationship_property_keys[,property]
      if(grepl("'", relationship_property_keys[,property], fixed = TRUE))
      {
        property_key_corrected = unlist(strsplit(as.character(relationship_property_keys[,property]), "'"))
        property_key_corrected = paste0(unlist(property_key_corrected), sep = "", collapse = "\\'")
      }

      query = paste0(query, property_key_corrected , "'")
      if(property != rev(names(relationship_property_keys))[1]){
        query = paste0(query, ", ")
      }
    }
    query = paste0(query, "}")
  }

  query = paste0(query, "]->(b {", end_node_property_corrected, ": '", end_node_property_value_corrected, "'}) DELETE r")
  result = call_neo4j(capture.output(cat(query)), connection, type = c("row", "graph"), output = c("r","json"), include_stats = FALSE, include_meta = FALSE)

  if(is.null(result$error_code)){
    print("Relationship has been deleted successfully")
  }
  return(result)
}
