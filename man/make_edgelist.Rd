% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/patchmax_dijsktra_func.R
\name{make_edgelist}
\alias{make_edgelist}
\title{Construct graph}
\usage{
make_edgelist(df, directed = TRUE, aux = NULL)
}
\arguments{
\item{df}{A data.frame or matrix containing 3 columns: from, to, cost. See details.}

\item{directed}{logical. If \code{FALSE}, then all edges are duplicated by inverting 'from' and 'to' nodes.}

\item{aux}{Optional. A vector or a single value describing an additional edge weight.}
}
\value{
Named list with two useful attributes for the user : \cr

\emph{nbnode} : total number of vertices \cr
\emph{dict$ref} : vertices IDs
}
\description{
Construct graph
}
\details{
'from' and 'to' are character or numeric vector containing nodes IDs.
'cost' is a non-negative numeric vector describing the cost (e.g time, distance) between each 'from' and 'to' nodes.
\code{aux} is an additional weight describing each edge. Shortest paths are always computed using 'cost' but \code{aux} can be summed over shortest paths.
}
