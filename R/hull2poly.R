#' Convert SpatialLines to SpatialPolygon
#'
#' @param sp_lines The SpatialLines object
#' @return The SpatialPolygon object created by those lines
#' @export
#' @examples

spLines2poly <- function(sp_lines){
  # Extract the lines slot
  lines_slot <- sp_lines@lines[[1]]
  # # Create a list of booleans indicating whether a given Line represents a polygon
  # poly_bool <- sapply(lines_slot@Lines, function(x){
  #   coords <- lines_slot@Lines[[1]]@coords
  #   # coords <- x@coords
  #   # Check if the first coordinate in the line is the same as the last
  #   all.equal(coords[1,], coords[nrow(coords),])
  # })
  # # Pull out the lines that form polygons
  # poly_lines <- sp_lines[poly_bool]
  # poly_lines_slot <- poly_lines@lines
  # Create SpatialPolygons
  sp_polys <- SpatialPolygons(list(Polygons(lapply(lines_slot@Lines, function(x) {
    Polygon(slot(x, "coords"))
  }
  ),
  ID = "1")))
  return(sp_polys)
}

#' Convert alpha hull to SpatialPolygon
#'
#' @param hull The alpha hull object
#' @return The SpatialPolygon object created by that alpha hull
#' @export
#' @examples
#' data(iris)
#' iris_sepals <- iris[,1:2]
#' iris_sepals <- iris_sepals[!duplicated(paste(iris_sepals$Sepal.Length, iris_sepals$Sepal.Width)), ]
#' alphahull_1 <- ahull(iris_sepals, alpha = 1)
#' hullpoly_1 <- ahull2poly(alphahull_1)
#' plot(iris_sepals, pch = 19, col = "darkseagreen")
#' plot(alphahull_1, lwd = 5, col = "gray", add = TRUE)
#' plot(hullpoly_1, border = "magenta", add = TRUE)

ahull2poly <- function(hull){
  # Convert the alpha hull to SpatialLines
  hull2SpatialLines <- ahull2lines(hull)
  # Convert SpatialLines to SpatialPolygon
  SpatialLines2SpatialPolygon <- spLines2poly(hull2SpatialLines)
  return(SpatialLines2SpatialPolygon)
}
