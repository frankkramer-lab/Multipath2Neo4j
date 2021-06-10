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
#'   node_row = nodes[row, names(nodes) != "label"]
#'   node_row = node_row[, !is.na(node_row)]
#'   createNode(connection,nodes[row,'label'],node_row)
#' }
createNode <- function(connection, node_label, property_keys = NULL) {
  #If the function is called without passing a neo4j connection, raise an exception
  if(missing(connection)){
    stop("Neo4j Connection must be specified")
  }
  #If the function is called without passing a node label, raise an exception
  if(missing(node_label)){
    stop("Node Label must be specified")
  }

  #If the property keys are passed as parameters, and if any property does not have a value, raise an exception
  if(!is.null(property_keys) && any(is.na(property_keys[,]))){
    stop("Property Keys must have values, not NA")
  }

  query = paste("CREATE (n:", node_label, sep="")

  if(!is.null(property_keys)){
    query = paste(query, "{", sep=" ")
    for(property in colnames(property_keys)){
      property_correct = gsub(" ", "_", property, fixed = TRUE)
      query = paste(query, property_correct , sep="")
      query = paste(query, ": '" , sep="")
      query = paste(query, property_keys[,property] , sep="")
      query = paste(query, "'" , sep="")
      if(property != rev(names(property_keys))[1]){
        query = paste(query, ", " , sep="")
      }
      else{
        query = paste(query, "}", sep="")
      }
    }
  }
  query = paste(query, ")", sep="")

  result = call_neo4j(query, connection, type = c("row", "graph"), output = c("r","json"), include_stats = FALSE, include_meta = FALSE)
  return(result)
}

