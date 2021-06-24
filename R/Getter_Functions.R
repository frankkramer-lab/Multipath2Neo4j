#' Get the number of labels in a Neo4j graph
#'
#' Get the number of labels in a Neo4j graph
#' @param connection The Neo4j connection object.
#'
#' @return The labels count in case of success, otherwise the result from the Neo4j call.
#'
#' @export
#' @import neo4r
#' @export
#'
#' @examples
#' #To get the labels count
#' result = getLabelsCount(connection)
getLabelsCount <- function(connection){
  #If the function is called without passing a neo4j connection, raise an exception
  if(missing(connection)){
    stop("Neo4j Connection must be specified")
  }
  if(connection$ping() == FALSE){
    stop("Check your connection")
  }

  query = "CALL db.labels()"

  result = call_neo4j(query, connection, type = c("row", "graph"), output = c("r","json"), include_stats = FALSE, include_meta = FALSE)

  if(is.null(result$error_code))
  {
    return(length(unlist(result)))
  }
  return(result)
}


#' Verify if the label exists in a graph
#'
#' Verify if the label exists in a graph
#' @param connection The Neo4j connection object.
#' @param label The name of the label.
#'
#' @return A boolean value in case of success, otherwise the result from the Neo4j call.
#'
#' @export
#' @import neo4r
#' @export
#'
#' @examples
#' #To get the labels count
#' isLabel(connection, "DrUGs")
isLabel <- function(connection, label){
  #If the function is called without passing a neo4j connection, raise an exception
  if(missing(connection)){
    stop("Neo4j Connection must be specified")
  }
  if(connection$ping() == FALSE){
    stop("Check your connection")
  }

  #If the function is called without passing a label, raise an exception
  if(missing(label) || is.null(label) || label == ""){
    stop("Node Label must be specified")
  }

  query = "CALL db.labels()"

  result = call_neo4j(query, connection, type = c("row", "graph"), output = c("r","json"), include_stats = FALSE, include_meta = FALSE)

  tmp = result
  if(is.null(result$error_code))
  {
    labels_names = lapply(unlist(result), function(x){tolower(x)})
    if (tolower(label) %in% labels_names) {
      tmp = TRUE
    }
    else{
      tmp = FALSE
    }
  }
  return(tmp)
}


#' Get the attributes of a node, or all nodes in a Neo4j graph
#'
#' Get the attributes of a node, or all nodes in a Neo4j graph
#' @param connection The Neo4j connection object.
#' @param property_key The node property key name to be identified by.
#' @param property_key_value The started node property key value to be found within.
#'
#' @return A dataframe containing the attributes of the specified node. If the node property key and value are not specified, all nodes with their attributes are returned, in case of success, otherwise the result from the Neo4j call.
#'
#' @export
#' @import neo4r
#' @export
#'
#' @examples
#' #To get all the nodes with their attributes
#' nodes = getNodeAttributes(connection)
#'
#' #To get the nodes of name Complex70 with its attributes
#' nodes = getNodeAttributes(connection,"name","Complex70")
getNodeAttributes <- function(connection, property_key = NULL, property_key_value = NULL){
  #If the function is called without passing a neo4j connection, raise an exception
  if(missing(connection)){
    stop("Neo4j Connection must be specified")
  }
  if(connection$ping() == FALSE){
    stop("Check your connection")
  }

  #If the function is called by passing one of the arguments property key and value, raise an exception
  if((missing(property_key) && !missing(property_key_value)) || (!missing(property_key) && missing(property_key_value))){
    stop("Property key and value must be both NULL or specified")
  }
  if((!missing(property_key) && !missing(property_key_value)) && ((property_key == "" && property_key_value != "") || (property_key != "" && property_key_value == ""))){
    stop("Property key and value must be both NULL or specified")
  }

  query = "MATCH (n"

  if(!is.null(property_key) && property_key != ""){
    query = paste0(query, " {", property_key, ": '", property_key_value ,"'}")
  }
  query = query = paste0(query, ") RETURN n")

  result = call_neo4j(query, connection, type = c("row", "graph"), output = c("r","json"), include_stats = FALSE, include_meta = FALSE)

  tmp = result
  if(is.null(result$error_code) && length(tmp))
  {
    tmp = as.data.frame(result, col.names = "")
  }
  return(tmp)
}


#' Get the nodes on a layer in a Neo4j graph
#'
#' Get the nodes on a layer in a Neo4j graph
#' @param connection The Neo4j connection object.
#' @param label The name of the label.
#' @param property_key The node property key name to be extracted.
#'
#' @return A list of nodes having the label, with their property key values.
#'
#' @export
#' @import neo4r
#' @export
#'
#' @examples
#' #To get the list of Proteins' names
#' result = getLabel(connection, "Protein", "name")
getLabel <- function(connection, label, property_key){
  #If the function is called without passing a neo4j connection, raise an exception
  if(missing(connection)){
    stop("Neo4j Connection must be specified")
  }
  if(connection$ping() == FALSE){
    stop("Check your connection")
  }

  #If the function is called without passing a label, raise an exception
  if(missing(label) || label == ""){
    stop("Label must be specified")
  }
  #If the function is called without passing a node property key, raise an exception
  if(missing(property_key) || property_key == ""){
    stop("Node Property Key must be specified")
  }

  query = paste0("MATCH (n:", label, ") RETURN n.", property_key)

  result = call_neo4j(query, connection, type = c("row", "graph"), output = c("r","json"), include_stats = FALSE, include_meta = FALSE)

  tmp = result
  if(is.null(result$error_code))
  {
    tmp = unlist(result, use.names = FALSE)
  }
  return(tmp)
}


#' Get the attributes of the relationships connecting two nodes, or all nodes in a Neo4j graph
#'
#' Get the attributes of the relationships connecting two nodes, or all nodes in a Neo4j graph
#' @param connection The Neo4j connection object.
#' @param returned_started_node_property The name of the attribute identifying the started node.
#' @param returned_end_node_property The name of the attribute identifying the end node.
#' @param started_node_property The started node property key name to be identified by.
#' @param started_node_property_value The started node property key value to be found within.
#' @param end_node_property The end node property key name to be identified by.
#' @param end_node_property_value The end node property key value to be found within.
#'
#' @return A dataframe containing the relationships with their attributes. If both nodes' properties and values are missing, it returns all the relationships with their attributes. If the started node's property name and value are given, it returns all the relationships starting from this node. If the end node's property name and value are given, it returns all the relationships ending with this node.
#'
#' @export
#' @import neo4r, dplyr
#' @export
#'
#' @examples
#' #To get all the edges with their attributes
#' edges = getRelationshipAttributes(connection, "name", "name")
#'
#' #To get all the edges starting from the node of name Complex1 with their attributes
#' edges = getRelationshipAttributes(connection, "name", "name", "name", "Complex1")
#'
#' #To get all the edges ending with the node of name Complex10 with their attributes
#' edges = getRelationshipAttributes(connection, "name", "name", end_node_property = "name", end_node_property_value = "Complex10")
getRelationshipAttributes <- function(connection, returned_started_node_property,
                                      returned_end_node_property,
                                      started_node_property = NULL,
                                      started_node_property_value = NULL,
                                      end_node_property = NULL,
                                      end_node_property_value = NULL){
  #If the function is called without passing a neo4j connection, raise an exception
  if(missing(connection)){
    stop("Neo4j Connection must be specified")
  }
  if(connection$ping() == FALSE){
    stop("Check your connection")
  }

  #If the function is called without passing the started node property key to be returned, raise an exception
  if(missing(returned_started_node_property) || is.null(returned_started_node_property) || returned_started_node_property == ""){
    stop("Started Node Property Key to be returned must be specified")
  }

  #If the function is called without passing the end node property key to be returned, raise an exception
  if(missing(returned_end_node_property) || is.null(returned_end_node_property) || returned_end_node_property == ""){
    stop("End Node Property Key to be returned must be specified")
  }

  if((is.null(started_node_property) && !is.null(started_node_property_value))
     ||
     (!is.null(started_node_property) && is.null(started_node_property_value))){
    stop("Started Node property key and value must be both NULL or specified.")
  }


  if((is.null(end_node_property) && !is.null(end_node_property_value))
     ||
     (!is.null(end_node_property) && is.null(end_node_property_value))){
    stop("End Node property key and value must be both NULL or specified.")
  }

  query = "MATCH (n"

  if(!is.null(started_node_property)){
    started_node_property_c = gsub(started_node_property, pattern = " ", replacement = "_")
    started_node_property_value_c = started_node_property_value

    if(grepl("'", started_node_property_value, fixed = TRUE))
    {
      started_node_property_value_c = unlist(strsplit(as.character(started_node_property_value), "'"))
      started_node_property_value_c = paste0(unlist(started_node_property_value_c), sep = "", collapse = "\\'")
    }
    query = paste0(query, " {", started_node_property_c, ": '",started_node_property_value_c, "'}")
  }

  query = paste0(query, ")-[r]->(m")

  if(!is.null(end_node_property)){
    end_node_property_c = gsub(end_node_property, pattern = " ", replacement = "_")
    end_node_property_value_c = end_node_property_value

    if(grepl("'", end_node_property_value, fixed = TRUE))
    {
      end_node_property_value_c = unlist(strsplit(as.character(end_node_property_value), "'"))
      end_node_property_value_c = paste0(unlist(end_node_property_value_c), sep = "", collapse = "\\'")
    }
    query = paste0(query, " {", end_node_property_c, ": '",end_node_property_value_c, "'}")
  }

  returned_started_node_property_c = gsub(returned_started_node_property, pattern = " ", replacement = "_")
  returned_end_node_property_c = gsub(returned_end_node_property, pattern = " ", replacement = "_")
  query = paste0(query, ") RETURN n.", returned_started_node_property_c, ",r,m.", returned_end_node_property_c)

  result = call_neo4j(query, connection, type = c("row", "graph"), output = c("r","json"), include_stats = FALSE, include_meta = FALSE)

  tmp = result
  if(is.null(result$error_code) && length(result) != 0)
  {

    tmp = as.data.frame(result)

    colnames(tmp)[which(names(tmp) == "value")] = "V1"
    colnames(tmp)[which(names(tmp) == "value.1")] = "V2"
    colnames(tmp)[startsWith(colnames(tmp),"r.")] = str_remove(colnames(tmp)[startsWith(colnames(tmp),"r.")],"r.")

    tmp = tmp %>% relocate(V1,V2)
  }
  return(tmp)
}
