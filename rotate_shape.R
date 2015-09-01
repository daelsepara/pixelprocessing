rotate_shape <- function(f, degree_angle, origin_x, origin_y) {
  
  # assumes vertices is a 3-column vector (x. y, gray level)
  rotated_ = array(0,dim(f))

  # center shape about origin
  theta_ = degree_angle*pi/180.0
  x_ = f[,1] - origin_x
  y_ = f[,2] - origin_y
  
  # perform rotation
  rotated_[,1] = x_*cos(theta_) - y_*sin(theta_)
  rotated_[,2] = x_*sin(theta_) + y_*cos(theta_)
  rotated_[,3] = f[,3]
  
  return(rotated_)
}
