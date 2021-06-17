#' Create a node with its property keys in Neo4j database
#'
#' Create a single node with its property keys, passed as arguments, in a Neo4j database.
#' @param connection The Neo4j connection object.
#' @param node_label The node label.
#' @param property_keys The node property keys, in the form of dataframe.
#'
#' @return The result from the Neo4j call.
#'
#' @export
#' @import neo4r
#' @export
#'
#' @examples
#' #Get the dataframe of nodes to be created
#' nodes = prepareNodesDataframeToCreate(wntBiopax,wntmully)
#'
#' #Create all nodes in the Neo4j database
#' for(row in 1:nrow(nodes)){
#'  node_property_keys = nodes[row, names(nodes) != "label"]
#'
#'  na_cells = which(is.na(node_property_keys))
#'  names_not_na = names(node_property_keys)[-na_cells]
#'  node_property_keys = as.data.frame(node_property_keys[,-na_cells])
#'  colnames(node_property_keys) = names_not_na
#'
#'  createNode(connection,nodes[row,'label'],node_property_keys)
#' }
createNode <- function(connection, node_label, property_keys = NULL) {
  #If the function is called without passing a neo4j connection, raise an exception
  if(missing(connection)){
    stop("Neo4j Connection must be specified")
  }
  #If the function is called without passing a node label, raise an exception
  if(missing(node_label) || is.null(node_label)){
    stop("Node Label must be specified")
  }

  #If the property keys are passed as parameters, and if any property does not have a value, raise an exception
  if(!is.null(property_keys) && any(is.na(property_keys[,]))){
    stop("Property Keys must have values, not NA")
  }
  if(connection$ping() == FALSE){
    stop("Check your connection")
  }

  query = paste0("CREATE (n:", node_label, sep = "")

  if(!is.null(property_keys)){
    query = paste0(query, " {", sep = "")
    for(property in colnames(property_keys)){
      query = paste0(query, property, ": '", sep = "")

      property_key_corrected = property_keys[,property]
      if(grepl("'", property_keys[,property], fixed = TRUE))
      {
        property_key_corrected = unlist(strsplit(as.character(property_keys[,property]), "'"))
        property_key_corrected = paste0(unlist(property_key_corrected), sep = "", collapse = "\\'")
      }

      query = paste0(query, property_key_corrected, sep = "")

      query = paste0(query, "'", sep = "")
      if(property != rev(names(property_keys))[1]){
        query = paste0(query, ", ", sep = "")
      }
    }
    query = paste0(query, "}", sep = "")
  }
  query = paste0(query, ")", sep = "")

  result = call_neo4j(query, connection, type = c("row", "graph"), output = c("r","json"), include_stats = FALSE, include_meta = FALSE)

  if(is.null(result$error_code))
  {
    print("Node Created Successfully")
  }
  else{
    print("Node Not Created")
  }
  return(result)
}


#' Create a relationship with its property keys in Neo4j database
#'
#' Create a single relationship with its property keys, passed as arguments, in a Neo4j database.
#' @param connection The Neo4j connection object.
#' @param started_node_label The started node label.
#' @param started_node_property The started node property key name to be identified by.
#' @param started_node_property_value The started node property key value to be found within.
#' @param end_node_label The end node label.
#' @param end_node_property The end node property key name to be identified by.
#' @param end_node_property_value The end node property key value to be found within.
#' @param relationship_label The relationship label.
#' @param relationship_property_keys The relationship property keys, in the form of dataframe.
#'
#' @return
#' @export
#' @import neo4r
#' @export
#'
#' @examples
#' #Get the dataframe of relationships to be created
#' relationships = prepareRelationshipsDataframeToCreate(wntmully,nodes)
#'
#'#Create all nodes in the Neo4j database
#'for(row in 1:nrow(relationships)){
#' relationship_property_keys = relationships[row, !names(relationships) %in% c("label", "V1 label", "V2 label", "V1", "V2")]
#'
#' na_cells = which(is.na(relationship_property_keys))
#' names_not_na = names(relationship_property_keys)[-na_cells]
#' relationship_property_keys = as.data.frame(relationship_property_keys[,-na_cells])
#' colnames(relationship_property_keys) = names_not_na
#'
#' createRelationship(connection, relationships[row,'V1_label'], 'name',
#'                     relationships[row,'V1'], relationships[row,'V2_label'],
#'                       'name', relationships[row,'V2'], relationships[row,'label'],
#'                          relationship_property_keys)
#'}
createRelationship <- function(connection,
                               started_node_label, started_node_property,
                               started_node_property_value,
                               end_node_label, end_node_property,
                               end_node_property_value,
                               relationship_label, relationship_property_keys = NULL){
  #If the function is called without passing a neo4j connection, raise an exception
  if(missing(connection)){
    stop("Neo4j Connection must be specified")
  }
  if(connection$ping() == FALSE){
    stop("Check your connection")
  }

  #If the function is called without passing the label of the started node, raise an exception
  if(missing(started_node_label) || is.null(started_node_label) || started_node_label == ""){
    stop("Started Node Label must be specified")
  }
  #If the function is called without passing a property of the started node, raise an exception
  if(missing(started_node_property) || is.null(started_node_property) || started_node_property == ""){
    stop("Started Node Property must be specified")
  }
  #If the function is called without passing the property's value of the started node, raise an exception
  if(missing(started_node_property_value) || is.null(started_node_property_value) || started_node_property_value == ""){
    stop("Started Node Property value must be specified")
  }

  #If the function is called without passing the label of the end node, raise an exception
  if(missing(end_node_label) || is.null(end_node_label) || end_node_label == ""){
    stop("End Node Label must be specified")
  }
  #If the function is called without passing a property of the end node, raise an exception
  if(missing(end_node_property) || is.null(end_node_property) || end_node_property == ""){
    stop("End Node Property must be specified")
  }
  #If the function is called without passing the property's value of the end node, raise an exception
  if(missing(end_node_property_value) || is.null(end_node_property_value) || end_node_property_value == ""){
    stop("End Node Property value must be specified")
  }

  #If the function is called without passing the Relationship label, raise an exception
  if(missing(relationship_label) || is.null(relationship_label) || relationship_label == ""){
    stop("Relationship Label must be specified")
  }

  query = paste0("MATCH (a:", started_node_label, "),(b:", end_node_label,
                 ") WHERE a.", started_node_property, " = '",
                 started_node_property_value, "' AND b.", end_node_property,
                 " = '", end_node_property_value, "' CREATE (a)-[r:",
                 relationship_label)


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
  query = paste0(query, "]->(b)")

  result = call_neo4j(query, connection, type = c("row", "graph"), output = c("r","json"), include_stats = FALSE, include_meta = FALSE)
  if(is.null(result$error_code))
  {
    print("Relationship Created Successfully")
  }
  else{
    print("Relationship Not Created")
  }

  return(result)
}
