# Function to convert an arc into line segments
# Given the center of the arc, the radius, the vector, and the angle (radians)

arc2line <- function(center, r, vector, theta, npoints) {
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
