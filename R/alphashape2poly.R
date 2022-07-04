#' Convert an alpha shape object to a polygon. Modified from https://rpubs.com/geospacedman/alphasimple.
#'
#' @param ashape An alpha shape object.
#' @return The polygon matching the boundaries of the alpha shape.
#' @export
#' @examples
#' set.seed(123)
#' x <- matrix(runif(100), nc = 2)
#' ashape_02 <- ashape(x, alpha = 0.2)
#' poly_result <- ashape2poly(ashape_02)

ashape2poly <- function(ashape){
  # Convert node numbers into characters
  ashape$edges[,1] <- as.character(ashape$edges[,1])
  ashape_graph <- igraph::graph_from_edgelist(ashape$edges[,1:2], directed = FALSE)
  if (!igraph::is.connected(ashape_graph)) {
    stop("Graph not connected")
  }
  if (any(igraph::degree(ashape_graph) != 2)) {
    stop("Graph not circular")
  }
  if (igraph::clusters(ashape_graph)$no > 1) {
    stop("Graph composed of more than one circle")
  }
  # Delete one edge to create a chain
  cut_graph <- ashape_graph - igraph::E(ashape_graph)[1]
  # Find chain end points
  ends <- names(which(igraph::degree(cut_graph) == 1))
  path <- igraph::get.shortest.paths(cut_graph, ends[1], ends[2])$vpath[[1]]
  # this is an index into the points
  pathX <- as.numeric(igraph::V(ashape_graph)[path]$name)
  # join the ends
  pathX <- c(pathX, pathX[1])
  # Subset the coordinates of points by pathX
  coords_subset <- ashape$x[pathX, ]
  p <- Polygon(coords_subset)
  ps <- Polygons(list(p),1)
  spatial_ps <- SpatialPolygons(list(ps))
  return(spatial_ps)
}
