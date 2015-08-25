mpq <- function(f, p_, q_) {
	
	size = dim(f)
	
	x = array(1:size[2])
	y = array(1:size[1])
	
	return(sum(outer(y^q_,x^p_)*f))
}

upq <- function(f, p_, q_) {

	m00 = mpq(f,0,0)
	x_ = mpq(f,1,0)/m00
	y_ = mpq(f,0,1)/m00
	
	size = dim(f)
	
	x = array(1:size[2])
	y = array(1:size[1])

	return(sum(outer((y-y_)^q_,(x-x_)^p_)*f))
}

npq <- function(f, p_, q_) {
	
	g = (p_ + q_)/2 + 1
	
	return(upq(f,p_,q_)/(upq(f,0,0)^g))
}

phi_f <- function(f, g) {
  
  phi_ = array(0,7)
  
  g02 = g(f,0,2)
  g03 = g(f,0,3)
  g11 = g(f,1,1)
  g12 = g(f,1,2)
  g20 = g(f,2,0)
  g21 = g(f,2,1)
  g30 = g(f,3,0)
  
  g411 = 4*g11
  g4112 = 4*g11^2
  g0312 = g03 + g12
  g2002 = g20 - g02
  g2103 = g21 + g03
  g3012 = g30 + g12
  g20022 = g2002^2
  g21032 = g2103^2
  g30122 = g3012^2
  g30312 = g30 - 3*g12
  g31230 = 3*g12 - g30
  g32103 = 3*g21 - g03
  g303122 = g30312^2
  g321032 = g32103^2
  g30122103 = g30122 - g21032
  g301232103 = g30122 - 3*g21032
  g33012221032 = 3*g30122 - g21032
  
  phi_[1] = g20 + g02
  
  phi_[2] = g20022 + g4112
  
  phi_[3] = g303122 + g321032
  
  phi_[4] = g30122 + g21032
  
  phi_[5] = g30312 * g3012 * g301232103 + g32103 * g2103 * g33012221032
  
  phi_[6] = g2002 * g30122103 + g411 * g3012 * g2103
  
  phi_[7] = g32103 * g3012 * g301232103 + g31230 * g2103 * g33012221032
  
  return(phi_)
}

phi <- function(f) {
  return(phi_f(f, npq))
}

phi_m <- function(f) {
  return(phi_f(f, mpq))
}
