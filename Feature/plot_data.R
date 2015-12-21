plot_data <- function (X, y, m, h = NULL, f = NULL) {

	##########################################################################
	# FUNCTION
	#   plot_data(X,y,m,h)
	# Plotting utility, capable of visualizing 2-dimensional datasets that
	# consist of, at most, 7 classes. It plots with different colors: (a) the 
	# vectors of a data set that belong to different classes and (b) the mean 
	# vectors of the classes, provided that the data are 2-dimensional and the 
	# total number of classes is at most 7.
	#
	# INPUT ARGUMENTS:
	#   X:  lxN matrix, whose columns are the data vectors to be plotted.
	#   y:  N-dimensional vector whose i-th component is the class label
	#       of the i-th data vector.
	#   m:  lxc matrix, whose j-th column corresponds to the
	#       mean vector of the j-th class.
	#   h:  the handle of the figure on which the data will be plotted.
	#
	# (c) 2010 S. Theodoridis, A. Pikrakis, K. Koutroumbas, D. Cavouras
	#
	# Converted and extended from Matlab into R by S.D. Separa (2015)
	#
	##########################################################################

	size_X = dim(X)
	size_m = dim(m)
	
	# N = no. of data vectors, l=dimensionality
	N = size_X[1]
	l = size_X[2]
	
	# c=no. of classes
	l = size_m[1]
	c_ = size_m[2]

	if (l != 2 || c_ > 7) {
		print('NO PLOT CAN BE GENERATED')
    } else {
		
		pale = c('red', 'green', 'blue', 'yellow', 'magenta', 'cyan', 'cyan')
		char = c(19, 19, 19, 19, 19, 19, 1)

		if (is.null(f)) {
			if (!is.null(h)) {
				dev.set(h)
			} else {
				dev.cur()
			}
		} else {
			if (!is.null(f)) {
				# save to png file
				png(f)
			}
		}
		
		# Adjust plot limits
		min_data = min(X, y, m)
		max_data = max(X, y, m)
		
		if (min_data >= 0.0) {
			min_data = 0.0
		} else {
			min_data = floor(min_data)
		}
		
		max_data = ceiling(max_data)

		# Plot of the data vectors
		plot(X[y == 1, 1], X[y == 1, 2], col = pale[1], pch = char[1], xlim = c(min_data, max_data), ylim = c(min_data, max_data), ann = FALSE)
		
		for (i in 2:c_) {
			points(X[y == i, 1], X[y == i, 2], col = pale[i], pch = char[i])
		}
    
		# Plot of the class centroids
		for (j in 1:c_) {
			points(m[1, j], m[2, j], col = 'black', pch = 3)
		}
		
		# save png file
		if (!is.null(f)) {
			dev.off()
		}
	}
}
