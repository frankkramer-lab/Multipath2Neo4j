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



