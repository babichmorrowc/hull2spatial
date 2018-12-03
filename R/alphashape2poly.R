# Function to convert alpha shape to a polygon
# Modified from https://rpubs.com/geospacedman/alphasimple

ashape2poly <- function(hull){
  # Convert node numbers into characters
  hull$ashape.obj$edges[,1] <- as.character(hull$ashape.obj$edges[,1])
  hull_graph <- graph_from_edgelist(hull$ashape.obj$edges[,1:2], directed = FALSE)
  if (!is.connected(hull_graph)) {
    stop("Graph not connected")
  }
  if (any(degree(hull_graph) != 2)) {
    stop("Graph not circular")
  }
  if (clusters(hull_graph)$no > 1) {
    stop("Graph composed of more than one circle")
  }
  # Delete one edge to create a chain
  cut_graph <- hull_graph - E(hull_graph)[1]
  # Find chain end points
  ends = names(which(degree(cut_graph) == 1))
  path = get.shortest.paths(cut_graph, ends[1], ends[2])$vpath[[1]]
  # this is an index into the points
  pathX = as.numeric(V(hull_graph)[path]$name)
  # join the ends
  pathX = c(pathX, pathX[1])
  return(pathX)
}
