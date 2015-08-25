# scale image by a factor of sf. can only handle natural numbers for now
magnify_image <- function(im, sf) {
	
	size = dim(im)
	y = size[1]
	x = size[2]
	
	magnifiedImage = array(0,size*sf)
	
	for (j in 1:y) {
		for (i in 1:x) {
			magnifiedImage[(sf*(j-1)+1):(sf*j),(sf*(i-1)+1):(sf*i)] = im[j,i]
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

# translate image to new origin
translate_image <- function(im, x, y) {

	size = dim(im)
	sf = 2;	sx = size[2]-1;	sy = size[1]-1
	
	while (((x+sx) > sf*size[2]) || ((y+sy) > sf*size[1])) {
		sf = sf + 1
	}
	
	newImage = array(0,size*sf)
	newImage[y:(y+sy),x:(x+sx)] = im
	
	return(newImage)
}
