
#' Generate vertex coordinates for a circle
#' 
circleVertices <- function(xc, yc, radius, npoints=25) {
  a <- seq(0, 2*pi, length.out = npoints + 1)
  x <- xc + radius * cos(a)
  y <- yc + radius * sin(a)
  m <- cbind("x" = x, "y" = y)
  return(m)
}
