#' Connect to Neo4j graph database
#'
#' @param url the Neo4j server url.
#' @param user The username of the Neo4j server ("neo4j" is the default user).
#' @param db The database name.
#'
#' @return A connection object to the Neo4j Server.
#' @export
#' @import neo4r
#' @export
#'
#' @examples
#' connection = connectToNeo4j("http://localhost:7474", "neo4j", "pathways")
#' To check if the connection succeeded, use connection$ping()
#' connection = connectToNeo4j()
connectToNeo4j <- function(url, user, db) {
  if(missing(url)){
    stop("URL must be specified")
  }
  if(missing(user)){
    stop("User must be specified")
  }
  if(missing(db)){
    stop("Database Name must be specified")
  }

  connection <- neo4j_api$new(
    url = url,
    user = user,
    password = rstudioapi::askForPassword(),
    db = db
    )

  return(connection)
}
