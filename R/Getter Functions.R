#' Get the number of labels in a graph
#'
#' Get the number of labels in a graph
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


