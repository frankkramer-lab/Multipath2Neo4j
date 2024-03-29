% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Getter_Functions.R
\name{getNodeAttributes}
\alias{getNodeAttributes}
\title{Get the attributes of a node, or all nodes in a Neo4j graph}
\usage{
getNodeAttributes(connection, property_key = NULL, property_key_value = NULL)
}
\arguments{
\item{connection}{The Neo4j connection object.}

\item{property_key}{The node property key name to be identified by.}

\item{property_key_value}{The started node property key value to be found within.}
}
\value{
A dataframe containing the attributes of the specified node. If the node property key and value are not specified,
all nodes with their attributes are returned, in case of success, otherwise the result from the Neo4j call.
}
\description{
Get the attributes of a node, or all nodes in a Neo4j graph
}
\examples{
\dontrun{
#To get all the nodes with their attributes
nodes = getNodeAttributes(connection)

#To get the nodes of name Complex70 with its attributes
nodes = getNodeAttributes(connection,"name","Complex70")
}
}
