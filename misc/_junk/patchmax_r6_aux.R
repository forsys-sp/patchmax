
#.....................................................................

#' Plot subgraph based on nearest vertices
#'
#' @param geom stand geometry
#' @param net igraph adjacency network
#' @param nn project dataframe
#' @param obj_field field name with objective values
#' @param search logical
#' @param plot logical
#' @param pname project name
#'
#' @return
#' @export

plot_project <- function(geom, net, nn, obj_field, search = NULL, plot=T, pname = NULL){
  
  geom_s <- geom |> filter(stand_id %in% V(net)$name)
  if(!is.null(search))
    geom_s <- geom_s |> mutate(!!obj_field := ifelse(search < 0, NA, search))
  
  proj_geom <- geom |> filter(stand_id %in% nn$node) 
  proj_geom_d <- proj_geom |> summarize() |> mutate(pname = pname)
  
  p <- ggplot() + 
    geom_sf(data = geom, aes(fill = get(obj_field)), alpha=0.6, linewidth = 0) +
    geom_sf(data = proj_geom, aes(fill = get(obj_field)), linewidth=0) + 
    geom_sf(data = proj_geom_d, fill=NA, color='black', linewidth=0.5, linetype=1) +
    geom_sf(data = geom_s |> filter(stand_id == nn$node[1]), fill='black', color='black', linewidth=1) +
    scale_fill_gradientn(colors = sf.colors(10)) + 
    cowplot::theme_map() + 
    theme(plot.title = element_text(hjust=0.5, vjust = -5, face = "plain")) +
    guides(fill = 'none')
  
  if(!is.null(V(net)$exclude))
    p <- p + geom_sf(
      data = filter(geom, stand_id %in% V(net)$name[which(V(net)$exclude == 1)]),
      fill = NA, color = 'red')
  
  if(plot){
    print(p)
  } else {
    return(p)
  }
}

#.....................................................................

#' Generate vertex coordinates for a circle
#' 
#' @param xc ???
#' @param yc ???
#' @param radius ??? 
#' @param npoints ???
#' 
circleVertices <- function(xc, yc, radius, npoints=25) {
  a <- seq(0, 2*pi, length.out = npoints + 1)
  x <- xc + radius * cos(a)
  y <- yc + radius * sin(a)
  m <- cbind("x" = x, "y" = y)
  return(m)
}