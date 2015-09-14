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

lbp_hist <- function(lbp_) {

	x_ = c(0, 1, 2, 3, 4, 6, 7, 8, 12, 14, 15, 16, 24, 28, 30, 31, 32, 48, 56, 60, 62, 63, 64, 96, 112, 120, 124, 126, 127, 128, 129, 131, 135, 143, 159, 191, 192, 193, 195, 199, 207, 223, 224, 225, 227, 231, 239, 240, 241, 243, 247, 248, 249, 251, 252, 253, 254, 255)
	
	hist_ = array(0, 59)
	
	for (i in 1:58) {
		hist_[i] = length(which(lbp_ == x_[i]))
	}
	
	hist_[59] = length(which(!(lbp_ %in% x_)))
	
	return(hist_)
}

lbp_histu <- function(u_) {

	hist_ = array(0, 8)
	
	for (i in 0:7) {
		hist_[i] = length(which(u_ == i))
	}
	
	return(hist_)
}
