magnify_shape <- function(f, sf) {

	# assumes: 
	# 
	# 1) f is a 3-column vector describing vertices x,y and gray level
	# 2) consecutive vertices in f are connected to each other

	size = dim(f);
	f_ = array(0,c(size[1]*sf,size[2]))

	# initialize origin
	origin_x = f[1, 1]
	origin_y = f[1, 2]

	for (i in 1:size[1]) {
		for (sf_ in 1:sf) {
			
			# stretch shape
			f_[sf*(i-1)+sf_, 1] = origin_x +(f[i %% size[1] + 1, 1]-f[i, 1])
			f_[sf*(i-1)+sf_, 2] = origin_y +(f[i %% size[1] + 1, 2]-f[i, 2])
			f_[sf*(i-1)+sf_, 3] = f[i, 3]
			
			# set new origin
			origin_x = f_[sf*(i-1) +sf_ ,1]
			origin_y = f_[sf*(i-1) +sf_ ,2]
		}
	}
	
	return(f_)
}
