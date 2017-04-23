cluster_iterative <- function(dx, dy, n_, dc_, png_file) {

	clusters = array(0, n_)
	temp = 1:n_
	
	# compute clusters based on Euclidean-distance
	while(!identical(clusters, temp))
	{
		clusters = temp
		
		for (n in 1:n_) {
			x_ = array(dx, n_) - dx[n]
			y_ = array(dy, n_) - dy[n]
			i_ = which(x_^2 + y_^2 <= dc_^2)
			temp[i_] <- min(temp[i_])
		}
	}
	
	unique_clusters = unique(clusters)

	new_clusters = array(0, n_)

	for (i in 1:length(unique_clusters)) {
		new_clusters[which(clusters == unique_clusters[i])]  = i
	}
	
	clusters = new_clusters
	
	colors_ = rainbow(length(unique_clusters))

	first_ = TRUE
	
	# determine plot limits
	ylimit = round(max(dy))
	xlimit = round(max(dx))

	if (!missing(png_file)) {
		png(png_file)
	}
	
	# plot clusters
	for (i in 1:length(unique_clusters)) {
		if (first_) {
			plot(dx[which(clusters == i)], dy[which(clusters == i)], col = colors_[i], xlab = 'x', ylab = 'y', xlim = c(0, xlimit), ylim = c(0, ylimit), pch = 19)
			first_ = FALSE
		} else {
			points(dx[which(clusters == i)], dy[which(clusters == i)], col = colors_[i], pch = 19)
		}
	}

	if (!missing(png_file)) {
		dev.off()
	}
	
	return(clusters)
}

iterative_cluster <- function (file_, x_c, y_c, range_, dc_, png_file) {

	data_ = read.table(file_)
	n_ = length(range_)
	
	dx = data_[range_, x_c]
	dy = data_[range_, y_c]
	
	return(cluster_iterative(dx, dy, n_, dc_, png_file))
}
