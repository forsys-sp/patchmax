#' Construct graph
#'
#' @param df  A data.frame or matrix containing 3 columns: from, to, cost. See details.
#' @param directed logical. If \code{FALSE}, then all edges are duplicated by inverting 'from' and 'to' nodes.
#' @param aux Optional. A vector or a single value describing an additional edge weight.
#' @return Named list with two useful attributes for the user : \cr
#'
#' \emph{nbnode} : total number of vertices \cr
#' \emph{dict$ref} : vertices IDs
#' @details 'from' and 'to' are character or numeric vector containing nodes IDs.
#' 'cost' is a non-negative numeric vector describing the cost (e.g time, distance) between each 'from' and 'to' nodes.
#' \code{aux} is an additional weight describing each edge. Shortest paths are always computed using 'cost' but \code{aux} can be summed over shortest paths.
#' 
#' @export

make_edgelist <- function(df, directed = TRUE, aux = NULL){
  
  df<-as.data.frame(df)
  if (ncol(df) != 3) stop("Data should have 3 columns")

  df[,1] <- as.character(df[,1])
  df[,2] <- as.character(df[,2])
  df[,3] <- as.numeric(df[,3])
  colnames(df)<-c("from","to","dist")

  nodes <- unique(c(df[,1],df[,2]))

  if (!is.null(aux)){
    if (length(aux) == 1) aux <- rep(aux, nrow(df))
    if (length(aux) != nrow(df)) stop("length(aux) must equal 1 or nrow(df)")
    if (sum(aux < 0) > 0) warning("aux contains negative values")
  }

  attrib <- list(aux)
  names(attrib) <- c("aux")

  if (directed == FALSE){
    df2 <- df[, c(2,1,3)]
    colnames(df2) <- colnames(df)
    df <- rbind(df, df2)
    attrib <- lapply(attrib, rep, 2)
  }

  dict <- data.frame(ref = nodes, id = 0:(length(nodes)-1), stringsAsFactors = F)
  df[,1] <- dict[match(df[,1], dict$ref), "id"]
  df[,2] <- dict[match(df[,2], dict$ref), "id"]

  out <- list(
    data = df,
    nbnode = length(nodes),
    dict = dict,
    attrib = attrib)
  
  return(out)

}
