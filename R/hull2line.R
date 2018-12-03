# Function to convert an arc into line segments
# Given the center of the arc, the radius, the vector, and the angle (radians)
arc2line <- function(center, r, vector, theta, npoints = 100) {
  # Get the angles at the extremes of the arcs
  angles <- anglesArc(vector, theta)
  # Generate sequence of angles along the arc to determine the points
  seqang <- seq(angles[1], angles[2], length = npoints)
  # Generate x coordinates for points along the arc
  x <- center[1] + r * cos(seqang)
  # Generate y coordinates for points along the arc
  y <- center[2] + r * sin(seqang)
  coords.xy <- cbind(x,y)
  line <- Line(coords = coords.xy)
  return(line)
}


# Function to convert alpha hulls into SpatialLines
ahull2lines <- function(hull){
  arclist <- hull$arcs
  lines <- list()
  for (i in 1:nrow(arclist)) {
    # Extract the attributes of arc i
    center_i <- arclist[i, 1:2]
    radius_i <- arclist[i, 3]
    vector_i <- arclist[i, 4:5]
    theta_i <- arclist[i, 6]
    # Convert arc i into a Line object
    line_i <- arc2line(center = center_i, r = radius_i, vector = vector_i, theta = theta_i)
    list_length <- length(lines)
    if(list_length > 0){
      # If a line has already been added to the list of lines
      # Define last_line_coords as the coordinates of the last line added to the list before the ith line
      last_line_coords <- lines[[list_length]]@coords
    }
    if(i == 1){
      # Add the first line to the list of lines
      lines[[i]] <- line_i
    } else if(all.equal(line_i@coords[1,], last_line_coords[nrow(last_line_coords),])){
      # If the first coordinate in the ith line is equal to the last coordinate in the previous line
      # then those lines should be connected
      # Row bind the coordinates for the ith line to the coordinates of the previous line in the list
      lines[[list_length]]@coords <- rbind(last_line_coords, line_i@coords[2:nrow(line_i@coords),])
    } else {
      # If the first coordinate in the ith line does not match the last coordinate in the previous line
      # then the ith line represents a new line
      # Add the ith line to the list as a new element
      lines[[length(lines) + 1]] <- line_i
    }
  }
  # Convert the list of lines to a Line object
  lines <- Lines(lines, ID = 'l')
  # Convert the Line object to a SpatialLines object
  sp_lines <- SpatialLines(list(lines))
  return(sp_lines)
}

