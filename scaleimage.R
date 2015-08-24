# scale image by a factor of sf. can only handle natural numbers for now
magnify_image <- function(im, sf) {
	
	size = dim(im)
	x = size[1]
	y = size[2]
	
	magnifiedImage = array(0,size*sf)
	
	for (j in 1:y) {
		for (i in 1:x) {
			magnifiedImage[(sf*(i-1)+1):(sf*i),(sf*(j-1)+1):(sf*j)] = im[i,j]
		}
	}
	
	return(magnifiedImage)
}

# increase image background
increase_background <- function(im, sf) {
	size = dim(im)
	newImage = array(0,size*sf)
	newImage[1:size[1],1:size[2]] = im
	return(newImage)
}
