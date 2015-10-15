pca_compress <- function(img_, k_, type_ = 'pca') {

	eigen_ob = eigen(cov(img_ - rowMeans(img_)))
	eigen_vectors = eigen_ob$vectors
	ev = eigen_vectors[,1:k_]
	
	if (type_ == 'values') {
		return(eigen_ob$values)
	}
	
	y = t(ev) %*% t(img_)
	x = t(ev %*% y) + rowMeans(img_)
	
	return(x)
}
