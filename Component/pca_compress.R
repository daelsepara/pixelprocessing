pca_compress <- function(img_, k_, type_ = 'pca') {

	# solve eigenvalue problem
	eigen_ob = eigen(cov(img_ - rowMeans(img_)))
	eigen_vectors = eigen_ob$vectors

	# return eigenvalues
	if (type_ == 'values') {
		return(eigen_ob$values)
	}
	
	# use desired number of PCA components (from eigenvectors)
	ev = eigen_vectors[, 1:k_]
	
	# project image using new basis functions
	y = t(ev) %*% t(img_)
	
	# reconstruct image
	x = t(ev %*% y) + rowMeans(img_)
	
	return(x)
}
