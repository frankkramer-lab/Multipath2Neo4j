% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Getter_Functions.R
\name{isLabel}
\alias{isLabel}
\title{Verify if the label exists in a graph}
\usage{
isLabel(connection, label)
}
\arguments{
\item{connection}{The Neo4j connection object.}

\item{label}{The name of the label.}
}
\value{
A boolean value in case of success, otherwise the result from the Neo4j call.
}
\description{
Verify if the label exists in a graph
}
\examples{
\dontrun{
#To get the labels count
isLabel(connection, "DrUGs")
}
}
