magnify_image <- function(im, sf) {
	
	size = dim(im)
	x = size[1]
	y = size[2]
	
	magnifiedImage = array(0,size*sf)
	
	for (j in 1:y) {
		for (i in 1:x) {
			magnifiedImage[(sf*(i-1)+1):(sf*i),(sf*(j-1)+1):(sf*j)] = un[i,j]
		}
	}
	
	return(magnifiedImage)
}
