# sigmoid activation function
h_func <- function(x) {

	return(1/(1 + exp(-x)))
}

# 1st-derivative of activation function
h_funcd <- function(x) {
	
	return(x * (1 - x))
}

# forward propagation
nnet_forward <- function(x, w_ji, w_kj) {

	j = length(w_kj)
	z_j = array(0, j)
	
	# compute activation at hidden layer with bias = 1.0
	for (jj in 1:j) {
		z_j[jj] = h_func(sum(x * w_ji[jj, ]) + 1.0)
	}
	
	# compute output with bias = 1.0
	y_k = h_func(sum(z_j * w_kj) + 1.0)
	
	return(list('y_k' = y_k, 'z_j' = z_j))
}

# backward propagation
nnet_backprop <- function (y_k, z_j, x, t_k, w_kj) {
	
	j = length(w_kj)
	i = length(x)
	
	dWji = array(0, c(j, i))
	dWkj = array(0, j)
	
	error_k = t_k - y_k
	delta_k = h_funcd(y_k)
	
	# compute weight updates based on Error and propagate backwards through the network
	dWkj =  error_k * delta_k * z_j
		
	for(jj in 1:j) {
		dWji[jj, ] =  error_k * delta_k * w_kj[jj] * h_funcd(z_j[jj]) * x
	}
	
	return(list('dWkj' = dWkj, 'dWji' = dWji, 'Error' = abs(error_k)))
}

# network training
nnet_train <-function(maxiter = 1000000, learning_rate = 0.1, tol = 10^(-3), training_set = array(c(0, 0, 1, 1, 0, 0, 0, 1), c(4, 2)) , output = c(0, 1, 1, 0), hidden_units = 4) {

	x = training_set
	
	# determine network dimensions from user input
	j = hidden_units
	inputs = dim(x)[2]
	patterns = dim(x)[1]
	
	# initialize training sets and outputs
	y_p = array(0, patterns)
	t_p = output
	
	
	# intialize interconnection weights with random values (-1, 1)
	w_ji = array(runif(n = j*inputs, min = -1, max = 1), c(j, inputs))
	w_kj = array(runif(n = j, min = -1, max = 1), j)
	
	# begin at a high error value
	Error = 1.0
	i = 0
	
	# training loop
	while (i < maxiter && Error > tol) {
	
		error_p = 0
		
		# batch processing
		for (p in 1:patterns) {
		
			# perform both forward and backpropagation tasks on each pattern
			forward = nnet_forward(x[p, ], w_ji, w_kj)
			backward = nnet_backprop(forward$y_k, forward$z_j, x[p, ], t_p[p], w_kj)
			
			# update weights
			w_ji = w_ji + learning_rate * backward$dWji
			w_kj = w_kj + learning_rate * backward$dWkj
			
			i = i + 1

			# save current performance
			error_p = error_p + backward$Error
			y_p[p] = forward$y_k
		}
		
		# compute average error
		Error = error_p / patterns
		
		if (i %% 100 == 0) {
			print(paste('iteration = ', i, ' Error = ', Error))
		}
	}
	
	return(list('y_p' = y_p, 'Error' = Error, 'iterations' = i, 'w_kj' = w_kj, 'w_ji' = w_ji))
}

