# sigmoid activation function
h_func <- function(x) {
  return(1/(1 + exp(-x)))
}

# 1st-derivative of activation function
h_funcd <- function(x) {
  z = h_func(x)
  return(z * (1 - z))
}

# forward propagation
nnet_forward <- function(training_set, w_ji, w_kj) {
	
	# add bias column to input layer
	x = cbind(array(1, c(nrow(training_set), 1)), training_set)
	
	# compute hidden layer activation
	z_2 = x %*% t(w_ji)
	z_j = h_func(z_2)
	
	# add bias column
	a_2 = cbind(array(1, c(nrow(z_j), 1)), z_j)
	
	# compute output layer
	y_k = h_func(a_2 %*% t(w_kj))
	
	return(list('y_k' = y_k, 'z_2' = z_2, 'a_2' = a_2))
}

# backward propagation
nnet_backprop <- function(training_set, y_k, z_2, a_2, w_ji, w_kj, y_matrix) {
	
	# add bias column
	x = cbind(array(1, c(nrow(training_set), 1)), training_set)
	m = nrow(x)
	
	cost = sum(-y_matrix * log(y_k) - (1 - y_matrix)*log(1 - y_k))/m
	
	# compute intermediate delta values per layer
	d3 = y_k - y_matrix
	d2 = d3 %*% w_kj[,2:ncol(w_kj)]*h_funcd(z_2)
	
	# compute gradient
	dWji = (t(d2) %*% x)/m
	dWkj = (t(d3) %*% a_2)/m
	
	return(list('dWkj' = dWkj, 'dWji' = dWji, 'Error' = cost))	
}

# network training
nnet_train <-function(maxiter = 100, learning_rate = 0.1, tol = 10^(-3), training_set = array(0) , output = array(0), hidden_units = 0, num_labels = 1, min_max = 1, isGaussian = FALSE) {

	# For multi-classification problem, format expected output
	# i.e. matrix, each row corresponds to a training pattern.
	# Each element in the row-vector is a 0 or 1 indicating whether
	# or not it belongs to that particular class
	
	if (num_labels > 1) {
		eye_matrix = diag(num_labels)
		y_matrix = eye_matrix[output, ]
	} else {
		# binary classification
		y_matrix = output
	}
	
	# determine network dimensions from user input
	j = hidden_units
	inputs = ncol(training_set)
  
	# intialize interconnection weights with random values (-min_max, min_max) or Gaussian (mean = 0, sd = min_max)
	if (!isGaussian) {
		w_ji = array(runif(n = j * (inputs + 1), min = -min_max, max = min_max), c(j, inputs + 1))
		w_kj = array(runif(n = num_labels * (j + 1), min = -min_max, max = min_max), c(num_labels, j + 1))
	} else {
		w_ji = array(rnorm(n = j * (inputs + 1), mean = 0, sd = abs(min_max)), c(j, inputs + 1))
		w_kj = array(rnorm(n = num_labels * (j + 1), mean = 0, sd = abs(min_max)), c(num_labels, j + 1))
	}
  
	iter = 0
	Error = 1.0
	
	y_k = numeric(0)
	
	while (iter < maxiter && Error > tol) {
	
		# for training, perform forward and backpropagation each iteration
		forward = nnet_forward(training_set, w_ji, w_kj)
		backward = nnet_backprop(training_set, forward$y_k, forward$z_2, forward$a_2, w_ji, w_kj, y_matrix)
		
		# update weights
		w_ji = w_ji - learning_rate * backward$dWji
		w_kj = w_kj - learning_rate * backward$dWkj
		
		# save current performance
		Error = backward$Error
		y_k = forward$y_k
    
		iter = iter + 1
		
		if (iter %% 1000 == 0) {
		  print(paste('iteration = ', iter, ' Error = ', Error))
		}
	}
	
	return(list('y_k' = y_k, 'Error' = Error, 'iterations' = iter, 'w_kj' = w_kj, 'w_ji' = w_ji))
}

# predict using neural network parameters (multi-class classification)
nnet_predict <- function(test_set, w_ji, w_kj) {

	output = nnet_forward(test_set, w_ji, w_kj)$y_k
	
	prediction = array(0, c(nrow(test_set), 1))
	
	if (ncol(output) > 1) {
		for (i in 1:nrow(test_set)) {
			prediction[i] = which.max(output[i, ])
		}
	} else {
		prediction = output
	}
	
	return(prediction)
}
