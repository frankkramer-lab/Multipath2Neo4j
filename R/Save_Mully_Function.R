#' Save a mully graph in the Neo4j database
#'
#' Save a mully graph in the Neo4j database
#' @param connection The Neo4j connection object.
#' @param biopax The biopax object.
#' @param mully The mully graph.
#'
#' @return A boolean value: TRUE if the mully graph has been saved in the Neo4j server. Otherwise FALSE.
#'
#' @export
#' @import neo4r, mully
#' @export
#'
#' @examples
#' saveMully(connection, wntBiopax, wntmully)
saveMully <- function(connection, biopax, mully){
  #If the function is called without passing a neo4j connection, raise an exception
  if(missing(connection)){
    stop("Neo4j Connection must be specified")
  }
  if(connection$ping() == FALSE){
    stop("Check your connection")
  }

  #If the function is called without passing a biopax object, raise an exception
  if(missing(biopax)){
    stop("Biopax Object must be specified")
  }

  #If the function is called without passing a mully object, raise an exception
  if(missing(mully)){
    stop("Mully Object must be specified")
  }

  try_result <- tryCatch(

    {
      #Get all mully nodes to be created
      nodes = prepareNodesDataframeToCreate(biopax, mully)

      #Create all nodes in the Neo4j database
      for(row in 1:nrow(nodes)){

        node_property_keys = nodes[row, names(nodes) != "label"]

        na_cells = which(is.na(node_property_keys))
        names_not_na = names(node_property_keys)[-na_cells]
        node_property_keys = as.data.frame(node_property_keys[,-na_cells])
        colnames(node_property_keys) = names_not_na

        createNode(connection, nodes[row,'label'], node_property_keys)

      }

      #If not all nodes are created, delete the created one
      if(row < nrow(nodes)){
        #Delete all nodes in the Neo4j database
        for(label in mully$layers$Name){
          deleteAllNodesOfLabel(connection,label)
        }
        return(FALSE)
      }

      #Get the dataframe of relationships to be created
      relationships = prepareRelationshipsDataframeToCreate(mully, nodes)

      #Create all nodes in the Neo4j database
      for(row in 1:nrow(relationships)){
        relationship_property_keys = relationships[row, !names(relationships) %in% c("label", "V1_label", "V2_label", "V1", "V2")]

        na_cells = which(is.na(relationship_property_keys))
        names_not_na = names(relationship_property_keys)[-na_cells]
        relationship_property_keys = as.data.frame(relationship_property_keys[,-na_cells])
        colnames(relationship_property_keys) = names_not_na

        createRelationship(connection, relationships[row,'V1_label'], 'name',
                           relationships[row,'V1'], relationships[row,'V2_label'],
                           'name', relationships[row,'V2'], relationships[row,'label'],
                           relationship_property_keys)
      }

      #If not all nodes are created, delete the created one
      if(row < nrow(nodes)){
        #Delete all nodes in the Neo4j database
        for(label in mully$layers$Name){
          deleteAllNodesOfLabel(connection,label)
        }
        return(FALSE)
      }
      cat("Mully Object has been saved in the Neo4j database successfully.")
      return(TRUE)
    },
    error = function(cond) {
      message(paste0("Here's the original error message:", cond))

      #Delete all nodes in the Neo4j database
      for(label in mully$layers$Name){
        deleteAllNodesOfLabel(connection,label)
      }

      return(FALSE)
    })
  return(try_result)
}
