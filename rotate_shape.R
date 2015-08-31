rotate_shape <- function(vertices, degree_angle, origin_x, origin_y) {
  
  # assumes vertices is a 2-column vector
  rotated_ = array(0,dim(vertices))

  # center shape about origin
  theta_ = degree_angle*pi/180.0
  x_ = vertices[,1] - origin_x
  y_ = vertices[,2] - origin_y
  
  # perform rotation
  rotated[,1] = x_*cos(theta_) - y_*sin(theta_)
  rotated[,2] = x_*sin(theta_) - y_*cos(theta_)
  
  return(rotated)
}