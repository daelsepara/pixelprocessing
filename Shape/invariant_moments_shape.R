mpq_xy <- function(f, p_, q_) {
	
	# assumes f is a 3-column matrix describing (x,y,binary values 0/1) of a shape
    return(sum(f[,1]^p_*f[,2]^q_*f[,3]))
}

upq_xy <- function(f, p_, q_) {
  
  m00 = mpq_xy(f,0,0)
  x_ = mpq_xy(f,1,0)/m00
  y_ = mpq_xy(f,0,1)/m00
  
  return(sum((f[,1]-x_)^p_*(f[,2]-y_)^q_*f[,3]))
}

npq_xy <- function(f, p_, q_) {
  
  g = (p_ + q_)/2 + 1
  
  return(upq_xy(f,p_,q_)/(upq_xy(f,0,0)^g))
}

# this just identical to the function phi_f at Shape/invariant_moments
phi_fxy <- function(f, g) {
  
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
  
  phi_[1:6] = abs(phi_[1:6])
  
  return(phi_)
}

phi_xy <- function(f) {
  return(phi_fxy(f, npq_xy))
}

cpq_xy <- function(f, p_, q_) {
  xy = complex(real = f[,1]-mean(f[,1]), imaginary = f[,2]-mean(f[,2]))
  return(sum(xy^p_*Conj(xy)^q_*f))
}

flusser_xy <- function(f) {
  
  phi_f = array(0,6)
  
  A = sum(f[,3])
  s11 = cpq_xy(f,1,1)/A^2
  s20 = cpq_xy(f,2,0)/A^2
  s21 = cpq_xy(f,2,1)/A^2.5
  s12 = cpq_xy(f,1,2)/A^2.5
  s30 = cpq_xy(f,3,0)/A^2.5
  
  phi_f[1] = Re(s11)
  phi_f[2] = Re(1000*s21*s12)
  phi_f[3] = 10000*Re(s20*s12*s12)
  phi_f[4] = 10000*Im(s20*s12*s12)
  phi_f[5] = 1000000*Re(s30*s12*s12*s12)
  phi_f[6] = 1000000*Im(s30*s12*s12*s12)
  
  return(phi_f)
}
