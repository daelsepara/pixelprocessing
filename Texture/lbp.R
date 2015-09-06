lbp <- function(f) {

	# weights of neighboring pixels
	#
	#   1|  2|  4
	# ------------
	# 128|  0|  8
	# ------------
	#  64| 32| 16
	w_ = c(1, 128, 64, 2, 0, 32, 4, 8, 16)
	
	lbp = array(0,dim(f))
	
	for(y in 2:(dim(f)[1]-1)) {
		for(x in 2:(dim(f)[2]-1)) {
		
			# apply threshold
			gn = as.vector(f[(y-1):(y+1),(x-1):(x+1)])
			i = which(gn >= f[y,x])
			
			# compute LBP based on weights
			lbp[y,x] = sum(w_[i])
		}
	}
	
	return(lbp)
}

