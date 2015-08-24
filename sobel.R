sobel <- function(f) {
	
	size = dim(f)
	edgeImage = array(0,size)

	# sobel filters
	xf = array(0,c(3,3))
	xf[,1] = c(-1, 0, 1)
	xf[,2] = c(-2, 0, 2)
	xf[,3] = c(-1, 0, 1)

	yf = array(0,c(3,3))
	yf[,1] = c(-1,-2,-1)
	yf[,2] = c( 0, 0, 0)
	yf[,3] = c( 1, 2, 1)
	
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
