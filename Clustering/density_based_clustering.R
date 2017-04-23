compute_cluster <- function(dx, dy, n_, dc_, png_file) {

	rho = density_rho(dx, dy, n_, dc_)
	d_i = dist_high_rho(dx, dy, n_, dc_, rho)
	
	sort_order = order(rho, decreasing = TRUE)
	
	# plot decision graph and get rho, d_i thresholds
	plot(rho, d_i)
	thresh = locator(1)
	
	centers = c()
	clusters = array(0, n_)

	# get cluster centers (isolated particles with very high rho and d_i)
	for (i in 1:n_) {
		if (rho[i] > thresh$x && d_i[i] > thresh$y) {
			centers = c(centers, i)
			clusters[i] = i
		}
	}
	
	delta = array(0, n_)
	nneigh = array(0, n_)
	
	delta[sort_order[1]] = -1.;
	nneigh[sort_order[1]] = sort_order[1];
	
	dij = array(0, c(n_, n_))
	
	# compute distance matrix
	for (i in 1:n_) {
		dij[i, ] = (dx - dx[i])^2 + (dy - dy[i])^2
	} 
	
	maxd = max(dij)

	# determine nearest neighbors starting from densest particles
	for (ii in 2:n_) {
		delta[sort_order[ii]] = maxd;
		for (jj in 1:(ii-1)) {
			if (dij[sort_order[ii], sort_order[jj]] < delta[sort_order[ii]]) {
				delta[sort_order[ii]] = dij[sort_order[ii], sort_order[jj]];
				nneigh[sort_order[ii]] = sort_order[jj];
			}
		}
	}
	
	# assign cluster
	for (i in 1:n_) {
		if (clusters[sort_order[i]] == 0) {
			clusters[sort_order[i]] = clusters[nneigh[sort_order[i]]]
		}
	}

	# re-label clusters
	unique_clusters = unique(clusters[which(clusters != 0)])
	
	new_clusters = array(0, n_)

	for (i in 1:length(unique_clusters)) {
		new_clusters[which(clusters == unique_clusters[i])]  = i
	}
	
	clusters = new_clusters
	
	# set plot limits
	ylimit = round(max(dy))
	xlimit = round(max(dx))

	if (!missing(png_file)) {
		png(png_file)
	}
	
	# plot clusters
	colors_ = rainbow(length(unique_clusters))
	
	first_ = TRUE
	
	for (i in 1:length(unique_clusters)) {
		if (first_) {
			plot(dx[which(clusters == i)], dy[which(clusters == i)], col = colors_[i], xlab = 'x', ylab = 'y', xlim = c(0, xlimit), ylim = c(0, ylimit), pch = 19)
			first_ = FALSE
		} else {
			points(dx[which(clusters == i)], dy[which(clusters == i)], col = colors_[i], pch = 19)
		}
	}

	# cluster-less
	points(points(dx[which(clusters == 0)], dy[which(clusters == 0)]))

	if (!missing(png_file)) {
		dev.off()
	}
	
	return(clusters)
}

density_cluster <- function(file_, x_c, y_c, range_, dc_, png_file) {

	# read data from file
	data_ = read.table(file_)
	dx = data_[range_, x_c]
	dy = data_[range_, y_c]
	n_ = length(range_)
	
	return(compute_cluster(dx, dy, n_, dc_, png_file))
}

compute_dist <- function(file_, x_c, y_c, range_, dc_, rho) {
	
	data_ = read.table(file_)

	# compute minium distance to denser particle
	return(dist_high_rho(data_[range_, x_c], data_[range_, y_c], length(range_), dc_, rho))
}

compute_rho <- function(file_, x_c, y_c, range_, dc_) {
	
	data_ = read.table(file_)
	
	# compute particle densities
	return(density_rho(data_[range_, x_c], data_[range_, y_c], length(range_), dc_))
}

euclidean_dist <- function(x_, y_, x_c, y_c) {
	return((x_ - x_c)^2 + (y_ - y_c)^2)
}

density_rho <- function(x_, y_, n_, dc_) {
	
	rho = array(0, n_)
	
	# compute particle densities
	for (i in 1:n_) {
		chi = (euclidean_dist(x_, y_, x_[i], y_[i]) - dc_^2)[-i]
		rho[i] = length(which(chi < 0))
	}
	
	return(rho)
}

dist_high_rho <-function(x_, y_, n_, dc_, rho) {

	# compute minimum distance to denser particle
	d_i = array(0, n_)
  
	for (i in 1:n_) {
		dij = (euclidean_dist(x_, y_, x_[i], y_[i]))[-i]
		pts = which(rho[-i] > rho[i])
    
		if (length(pts) > 0) {
			d_i[i] = min(dij[pts])
		} else {
			# densest particle
			d_i[i] = max(dij)
		}
	}
  
	return(d_i)
}

