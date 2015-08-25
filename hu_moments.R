# adapted from http://limitless-thoughts.blogspot.com/2011/05/hus-seven-moments-invariant-matlab-code.html

hu_moments <- function(f) {
	size = dim(f)
	x = as.vector(outer(array(0,size[1]),array(1:size[2]),FUN='+'))
	y = as.vector(outer(array(1:size[1]),array(0,size[2]),FUN='+'))
	f_ = as.vector(f)

	m00 = sum(f_)
	m10 = sum(x*f_)
	m01 = sum(y*f_)
	m11 = sum(x*y*f_)
	m20 = sum(x^2*f_)
	m02 = sum(y^2*f_)
	m30 = sum(x^3*f_)
	m03 = sum(y^3*f_)
	m12 = sum(x*y^2*f_)
	m21 = sum(x^2*y*f_)
	
	xbar = m10/m00
	ybar = m01/m00

	eta11 = (m11-ybar*m10)/m00^2
	eta20 = (m20-xbar*m10)/m00^2
	eta02 = (m02-ybar*m01)/m00^2
	eta30 = (m30-3*xbar*m20+2*xbar^2*m10)/m00^2.5
	eta03 = (m03-3*ybar*m02+2*ybar^2*m01)/m00^2.5
	eta21 = (m21-2*xbar*m11-ybar*m20+2*xbar^2*m01)/m00^2.5
	eta12 = (m12-2*ybar*m11-xbar*m02+2*ybar^2*m10)/m00^2.5
	
	phi = array(0,7)
	phi[1] = eta20+eta02
	phi[2] = (eta20-eta02)^2+4*eta11^2
	phi[3] = (eta30-3*eta12)^2+(3*eta21-eta03)^2
	phi[4] = (eta30+eta12)^2+(eta21+eta03)^2
	phi[5] = (eta30-3*eta12)*(eta30+eta12)*((eta30+eta12)^2-3*(eta21+eta03)^2)+(3*eta21-eta03)*(eta21+eta03)*(3*(eta30+eta12)^2-(eta21+eta03)^2)
	phi[6] = (eta20-eta02)*((eta30+eta12)^2-(eta21+eta03)^2)+4*eta11*(eta30+eta12)*(eta21+eta03)
	phi[7] = (3*eta21-eta03)*(eta30+eta12)*((eta30+eta12)^2-3*(eta21+eta03)^2)+(3*eta12-eta30)*(eta21+eta03)*(3*(eta30+eta12)^2-(eta21+eta03)^2)
	
	return(phi)
}
