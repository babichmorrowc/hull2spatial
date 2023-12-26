#' Convert ahull.IUCN to a SpatialLines or sf LINESTRING object.
#'
#' @param iucn_hull The ahull.IUCN object
#' @param sp_or_sf Either "sp" or "sf" depending on the type of object you want to return.
#'
#' @return Either an object of class "SpatialLines", if [[sp_or_sf]] is "sp" or of class "sf" if [[sp_or_sf]] is "sf".
#' @export
#'
ahull.IUCN2lines <- function(iucn_hull, sp_or_sf) {
  from_to <- iucn_hull[["bd.ah.IUCN"]]
  line_list <- list()
  for (i in 1:nrow(from_to)) {
    from_i <- from_to[i, 1]
    to_i <- from_to[i, 2]
    line_i_coords <- rbind(iucn_hull$x[from_i,],
                           iucn_hull$x[to_i,])
    line_i <- sp::Line(line_i_coords)
    line_list <- append(line_list, line_i)
  }
  lines <- sp::Lines(line_list, ID = '1')
  sp_lines <- sp::SpatialLines(list(lines))
  st_lines <- sf::st_as_sf(sp_lines)
  merged_lines <- sf::st_line_merge(st_lines)
  if(sp_or_sf == "sp") {
    sp_merged_lines <- sf::as_Spatial(merged_lines)
    return(sp_merged_lines)
  } else if(sp_or_sf == "sf") {
    return(merged_lines)
  } else {
    stop("sp_or_sf must be either sp or sf")
  }
}

#' Convert ahull.IUCN to a SpatialLines or sf POLYGON object.
#'
#' @param iucn_hull The ahull.IUCN object
#' @param sp_or_sf Either "sp" or "sf" depending on the type of object you want to return.
#'
#' @return Either an object of class "SpatialPolygons", if [[sp_or_sf]] is "sp" or of class "sf" if [[sp_or_sf]] is "sf".
#' @export
#'
ahull.IUCN2poly <- function(iucn_hull, sp_or_sf) {
  hull2lines <- ahull.IUCN2lines(iucn_hull, sp_or_sf)
  if(sp_or_sf == "sp") {
    SpatialLines2SpatialPolygon <- spLines2poly(hull2lines)
    return(SpatialLines2SpatialPolygon)
  } else if(sp_or_sf == "sf") {
    sf_poly <- sf::st_cast(hull2lines, "POLYGON")
    return(sf_poly)
  } else {
    stop("sp_or_sf must be either sp or sf")
  }
}
