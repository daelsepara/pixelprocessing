# generic edge-detection function, input operator
edge_detect <- function(f, xf, yf) {
	size = dim(f)
	edgeImage = array(0,size)

	for (j in 2:(size[2]-1)) {
		for (i in 2:(size[1]-1)) {
			gx = sum(-f[(i-1):(i+1),(j-1):(j+1)]*xf)
			gy = sum(-f[(i-1):(i+1),(j-1):(j+1)]*yf)
			g_ = sqrt(gx*gx + gy*gy)
			
			if (g_ > 255) g_ = 255
			if (g_ < 0) g_ = 0

			edgeImage[i,j] = g_
		}
	}
	
	return(edgeImage)
}

sobel <- function(f) {
	# Sobel operator
	xf = array(0,c(3,3))
	xf[,1] = c(-1, 0, 1)
	xf[,2] = c(-2, 0, 2)
	xf[,3] = c(-1, 0, 1)

	yf = array(0,c(3,3))
	yf[,1] = c(-1,-2,-1)
	yf[,2] = c( 0, 0, 0)
	yf[,3] = c( 1, 2, 1)

	return(edge_detect(f,xf,yf))
}

sobel_feldman <- function(f) {
	# Sobel-Feldman operator
	xf = array(0,c(3,3))
	xf[,1] = c( 3, 10, 3)
	xf[,2] = c( 0,  0, 0)
	xf[,3] = c(-3,-10,-3)

	yf = array(0,c(3,3))
	yf[,1] = c( 3, 0, -3)
	yf[,2] = c(10, 0,-10)
	yf[,3] = c( 3, 0, -3)

	return(edge_detect(f,xf,yf))
}

prewitt <- function(f) {
	# Prewitt operator
	xf = array(0,c(3,3))
	xf[,1] = c(-1, 0, 1)
	xf[,2] = c(-1, 0, 1)
	xf[,3] = c(-1, 0, 1)

	yf = array(0,c(3,3))
	yf[,1] = c(-1,-1,-1)
	yf[,2] = c( 0, 0, 0)
	yf[,3] = c( 1, 1, 1)

	return(edge_detect(f,xf,yf))
}
