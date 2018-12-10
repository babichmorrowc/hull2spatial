#' Convert an alpha shape object to a polygon.
#'
#' @param ashape An alpha shape object.
#' @return The polygon matching the boundaries of the alpha shape.
#' @examples
#' add(1, 1)
#' add(10, 1)

# Function to convert alpha shape to a polygon
# Modified from https://rpubs.com/geospacedman/alphasimple

ashape2poly <- function(ashape){
  # Convert node numbers into characters
  ashape$ashape.obj$edges[,1] <- as.character(ashape$ashape.obj$edges[,1])
  ashape_graph <- graph_from_edgelist(ashape$ashape.obj$edges[,1:2], directed = FALSE)
  if (!is.connected(ashape_graph)) {
    stop("Graph not connected")
  }
  if (any(degree(ashape_graph) != 2)) {
    stop("Graph not circular")
  }
  if (clusters(ashape_graph)$no > 1) {
    stop("Graph composed of more than one circle")
  }
  # Delete one edge to create a chain
  cut_graph <- ashape_graph - E(ashape_graph)[1]
  # Find chain end points
  ends = names(which(degree(cut_graph) == 1))
  path = get.shortest.paths(cut_graph, ends[1], ends[2])$vpath[[1]]
  # this is an index into the points
  pathX = as.numeric(V(ashape_graph)[path]$name)
  # join the ends
  pathX = c(pathX, pathX[1])
  return(pathX)
}
