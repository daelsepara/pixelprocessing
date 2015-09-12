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

color_hist <- function(ncc_, bins) {

	rint = round(ncc_[, , 1]*(bins-1) + 1)
	gint = round(ncc_[, , 2]*(bins-1) + 1)
 
	cols_ = as.vector(gint) + (as.vector(rint)-1)*bins
	hist_ = array(0, c(bins,bins))
	
	for (row_ in 1:bins) {
		for(col_ in 1:(bins - row_ + 1)) {
			hist_[row_, col_] = length(which(cols_ == (col_ + (row_-1)*bins)));
		}
	}
	
	return(hist_)
}

backpropagate_hist <- function(img_, hist_, bins) {

	size = dim(img_)
	
	rows_ = size[1];
	cols_ = size[2];
 
	backproj = array(0,c(rows_,cols_));
	
	for (i in 1:rows_) {
		for (j in 1:cols_) {
			
			r_new = round(img_[i, j, 1]*(bins-1) + 1)
			g_new = round(img_[i, j, 2]*(bins-1) + 1)
			
			backproj[i, j] = hist_[r_new, g_new]
      
		}
	}
	
	return(backproj)
}
