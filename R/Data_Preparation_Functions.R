#' Prepare the nodes dataframe
#'
#' Prepare a dataframe containing the nodes of a mully graph, extracted from getNodesAttribues() function, to be created in Neo4j database. The dataframe contains the nodes' label and their property keys.
#' @param biopax The biopax object.
#' @param g The mully graph.
#'
#' @return A dataframe of nodes ready to be created in Neo4j database.
#' @export
#' @import mully
#' @export
#'
#' @examples
#' nodes = prepareNodesDataframeToCreate(wntBiopax,wntmully)
prepareNodesDataframeToCreate <- function(biopax, g){
  #If the function is called without passing a biopax object, raise an exception
  if(missing(biopax)){
    stop("Biopax Object must be specified")
  }
  #If the function is called without passing a mully graph, raise an exception
  if(missing(g)){
    stop("Mully Graph must be specified")
  }

  #Get all the nodes of the mully graph
  result = mully::getNodeAttributes(g)

  #Get all the layers of the mully graph
  node_layers = g$layers

  #Put the layer of each node in its row
  result$label = node_layers$Name[result$n]

  #Split databases' ids
  result = splitDBIDs(biopax, result)

  #Remove the column "n" from the dataframe containing the nodes
  result = result[ , !(names(result) %in% c("n", "id","database"))]

  #Remove spaces from columns names (Neo4j does not accept spaces in property keys names)
  names(result) = gsub(names(result), pattern = " ", replacement = "_")

  #Replace all double quotes with single quotes in the dataframe for creation node query
  result = data.frame(lapply(result, function(x) {gsub('"', "'", x)}))

  return(result)
}


#' Split the databases' ids' of the nodes extracted from the function getNodesAttributes()
#'
#' @param biopax The biopax object.
#' @param nodes_df The nodes dataframe.
#'
#' @return A dataframe containing the nodes with their ids in each database respectively.
#'
#' @import stringi
#'
#' @examples
#' nodes_dataframe = splitDBIDs(biopax, nodes_dataframe)
splitDBIDs <- function(biopax, nodes_df){
  if(missing(biopax)){
    stop("Biopax object must be specified")
  }
  if(missing(nodes_df)){
    stop("Nodes dataframe must be specified")
  }
  #Get all the databases names listed in the nodes dataframe
  database_names = unique(unlist(strsplit(as.character(nodes_df$'database'), ",")))

  #Append " ID" to the databases names
  db_col_names = paste(database_names, "ID", sep=" ")

  #Copy the nodes dataframe into a new dataframe, and create new column for each database name having value NA
  result = nodes_df
  result[,db_col_names] = NA

  #Fill external ids of each node in the extracted databases
  for(row in 1:nrow(result)){
    #If the current row does not have any id for any database, skip this row
    if(is.na(result[row,'id'])){
      next
    }
    else{
      for (db in database_names){
        mapping_col_name = paste(db, "id", sep=" ")

        #Get the external ids of the node from the biopax object
        external_ids_df = getExternalIDs(biopax, result[row,'name'], db)

        #If the node has ids for the current db, join them all with ","
        if(nrow(external_ids_df) != 0){
          external_ids_str = stri_join_list(external_ids_df[mapping_col_name], sep = ",", collapse = NULL)
          col_name = paste(db, "ID", sep=" ")
          result[row,col_name] = external_ids_str
        }
      }
    }
  }
  #Remove the columns "id" and "database" from the dataframe containing the nodes
  result = result[ , !(names(result) %in% c("id","database"))]
  return(result)
}


#' Prepare the relationships dataframe
#'
#' Prepare a dataframe containing the relationships of a mully graph, extracted from getEdgesAttribues() function, to be created in Neo4j database. The dataframe contains the relationships' label, extremities nodes' labels, relationships' labels and their property keys.
#' @param g The mully graph.
#' @param nodes The nodes dataframe, containing the nodes' labels
#'
#' @return A dataframe of relationships ready to be created in Neo4j database.
#' @export
#' @import mully
#' @export
#'
#' @examples
#' #Get the dataframe of nodes to be created
#' relationships = prepareRelationshipsDataframeToCreate(wntmully,nodes)
prepareRelationshipsDataframeToCreate <- function(g, nodes){
  #If the function is called without passing a mully graph, raise an exception
  if(missing(g)){
    stop("Mully Graph must be specified")
  }
  #If the function is called without passing a nodes dataframe, raise an exception
  if(missing(nodes)){
    stop("Nodes dataframe must be specified")
  }
  #If the function is called with a nodes dataframe missing a label column, raise an exception
  if(!'label' %in% colnames(nodes)){
    stop("Nodes dataframe must contain a column called 'label")
  }

  #Get all edges of the mully graph
  result = getEdgeAttributes(g)

  #Put the label of V1 and Vo2 nodes in each row
  result$'V1 label' = nodes$'label'[match(result$'V1', nodes$'name')]
  result$'V2 label' = nodes$'label'[match(result$'V2', nodes$'name')]

  #Put the relationship type in each row (if V1 and V2 have the same label "intra", otherwise "inter")
  result$'label' = ifelse(result$`V1 label` == result$`V2 label`, 'intra', 'inter')

  #Remove spaces from columns names (Neo4j does not accept spaces in property keys names)
  names(result) = gsub(names(result), pattern = " ", replacement = "_")

  #Replace all double quotes with single quotes in the dataframe for creation relationship query
  result = data.frame(lapply(result, function(x) {gsub('"', "'", x)}))

  return(result)
}
