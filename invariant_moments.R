mpq <- function(f, p_, q_) {
	
	size = dim(f)
	
	x = array(1:size[1])
	y = array(1:size[2])
	
	return(sum(outer(x^p_,y^q_)*f))
}

upq <- function(f, p_, q_) {

	m00 = mpq(f,0,0)
	x_ = mpq(f,1,0)/m00
	y_ = mpq(f,0,1)/m00
	
	size = dim(f)
	
	x = array(1:size[1])
	y = array(1:size[2])

	return(sum(outer((x-x_)^p_,(y-y_)^q_)*f))
}

npq <- function(f, p_, q_) {
	
	g = (p_ + q_)/2 + 1
	
	return(upq(f,p_,q_)/(upq(f,0,0)^g))
}

phi <- function(f) {

	phi_ = array(0,7)
	
	n02 = npq(f,0,2)
	n03 = npq(f,0,3)
	n11 = npq(f,1,1)
	n12 = npq(f,1,2)
	n20 = npq(f,2,0)
	n21 = npq(f,2,1)
	n30 = npq(f,3,0)
	
	n411 = 4*n11
	n4112 = 4*n11^2
	n0312 = n03 + n12
	n2002 = n20 - n02
	n2103 = n21 + n03
	n3012 = n30 + n12
	n20022 = n2002^2
	n21032 = n2103^2
	n30122 = n3012^2
	n30312 = n30 - 3*n12
	n31230 = 3*n12 - n30
	n32103 = 3*n21 - n03
	n303122 = n30312^2
	n321032 = n32103^2
	n30122103 = n30122 - n21032
	n301232103 = n30122 - 3*n21032
	n33012221032 = 3*n30122 - n21032
	
	phi_[1] = n20 + n02

	phi_[2] = n20022 + n4112

	phi_[3] = n303122 - n321032

	phi_[4] = n30122 + n21032
	
	phi_[5] = n30312 * n3012 * n301232103 + n32103 * n2103 * n33012221032
	
	phi_[6] = n2002 * n30122103 + n411 * n3012 * n2103
	
	phi_[7] = n32103 * n3012 * n301232103 + n31230 * n2103 * n33012221032
	
	return(phi_)
}
