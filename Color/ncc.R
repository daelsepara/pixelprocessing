ncc <- function(rgb_) {
	
	size = dim(rgb_)
	
	rg_ = array(0, c(size[1], size[2], 2))
	
	for (y in 1:size[1]) {
		for (x in 1:size[2]) {
			
			i_ = sum(rgb_[y, x,])
			
			rg_[y, x, 1] = rgb_[y, x, 1]/i_
			rg_[y, x, 2] = rgb_[y, x, 2]/i_
		}
	}
	
	return(rg_)
}

color_hist <- function(ncc_, bins) {

	rint = round(ncc_[, , 1]*(bins - 1) + 1)
	gint = round(ncc_[, , 2]*(bins - 1) + 1)
 
	cols_ = as.vector(gint) + (as.vector(rint) - 1)*bins
	
	hist_ = array(0, c(bins, bins))
	
	for (row_ in 1:bins) {
		for(col_ in 1:(bins - row_ + 1)) {
		
			hist_[row_, col_] = length(which(cols_ == (col_ + (row_-1)*bins)))
			
		}
	}
	
	return(hist_)
}

backpropagate_hist <- function(ncc_, hist_, bins) {

	size = dim(ncc_)
	
	backp_ = array(0, c(size[1], size[2]))
	
	for (y in 1:size[1]) {
		for (x in 1:size[2]) {
			
			r_ = round(ncc_[y, x, 1]*(bins - 1) + 1)
			g_ = round(ncc_[y, x, 2]*(bins - 1) + 1)
			
			backp_[y, x] = hist_[r_, g_]
      
		}
	}
	
	return(backp_)
}
