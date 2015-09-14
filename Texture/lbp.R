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
	
	size = dim(f)
	
	for(y in 2:(size[1]-1)) {
		for(x in 2:(size[2]-1)) {
		
			# apply threshold
			gn = as.vector(f[(y-1):(y+1),(x-1):(x+1)])
			i = which(gn >= f[y,x])
			
			# compute LBP based on weights
			lbp[y,x] = sum(w_[i])
		}
	}
	
	return(lbp[2:(size[1]-1),2:(size[2]-1)])
}

bit_transitions <- function(i_) {

	dir_ = bitwAnd(i_, 1)
	
	transitions = 0
	
	for (i in 1:7) {
		
		dir_n = bitwAnd(bitwShiftR(i_, i), 1)
		
		if (dir_n != dir_) {
			transitions = transitions + 1
			dir_ = dir_n
		}
	}
	
	return(transitions)
}

u_lbp <- function(lbp_) {
	
	size = dim(lbp_)
	
	U_ = array(0,size)
	
	# count number of bit transitions in a table via lookup table
	for(y in 1:size[1]) {
		for(x in 1:size[2]) {
		
			# compute number of bit transitions
			U_[y,x] = bit_transitions(lbp_[y,x])
		}
	}
	
	return(U_)
}
