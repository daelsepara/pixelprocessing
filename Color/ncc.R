ncc <- function(rgb_) {
	size = dim(rgb_)
	
	rg_ = array(0, c(size[1],size[2],2))
	
	for (y in 1:size[1]) {
		for (x in 1:size[2]) {
			
			I_ = sum(rgb_[y, x,])
			
			rg_[y, x, 1] = rgb_[y, x, 1]/I_
			rg_[y, x, 2] = rgb_[y, x, 2]/I_
		}
	}
	
	return(rg_)
}
