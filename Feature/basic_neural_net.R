# sigmoid activation function
h_func <- function(x) {

	return(1/(1+exp(-x)))
}

# 1st-derivative of activation function
h_funcd <- function(x) {

	return(x*(1-x))
}

# boolean gate operation (defaults to XOR operation)
boolean_gate <- function(output = c(0, 1, 1, 0)) {

	booldata = array(0, c(4, 2))
	booldata[1, ] = c(0, 0)
	booldata[2, ] = c(0, 1)
	booldata[3, ] = c(1, 0)
	booldata[4, ] = c(1, 1)
	
	return(list('patterns' = booldata, 'output' = output))
}

# feed-forward operation
nnet_forward <- function(x, w_ji, w_kj) {
	
	# activation function with bias = 1.0
	z_j = h_func(x %*% w_ji + 1.0)
	y_k = h_func(z_j %*% w_kj + 1.0)
	
	return(list('y_k' = y_k, 'z_j' = z_j))
}

# backward propagation
nnet_backprop <- function (y_k, z_j, x, t_k, w_kj) {
	
	error_k = t_k - y_k
	delta_k = error_k*h_funcd(y_k)

	error_j = delta_k %*% (t(w_kj))
	delta_j = error_j * h_funcd(z_j)

	# compute delta weight updates
	dWkj = t(z_j) %*% (delta_k)
	dWji = t(x) %*% (delta_j)

	# compute mean error
	Error = mean(abs(error_k))

	return(list('dWkj' = dWkj, 'dWji' = dWji, 'Error' = Error))
}

nnet_train <-function(maxiter = 1000000, learning_rate = 0.1, tol = 10^(-3), output = c(0, 1, 1, 0), Gaussian = FALSE, mu = 0.0, sigma = 1.0) {
	
	boolean_op = boolean_gate(output)
	
	x = boolean_op$patterns
	t_k = boolean_op$output
	
	ii = dim(x)
	
	Error = 4
	
	# intialize weights
	if (!Gaussian) {
		w_ji = array(runif(n = length(ii), min = -1, max = 1), rev(ii))
		w_kj = runif(n = ii[1], min = -1, max = 1)
	} else {
		w_ji = array(rnorm(n = length(ii), mean = mu, sd = sigma), rev(ii))
		w_kj = rnorm(n = ii[1], mean = mu, sd = sigma)
	}
	
	i = 0
	
	convergence = numeric(0)
	y_k = array(0, length(t_k))
	
	# training loop
	while (i < maxiter && Error > tol) {
		
		forward = nnet_forward(x, w_ji, w_kj)
		backward = nnet_backprop(forward$y_k, forward$z_j, x, t_k, w_kj)
		
		# update weights
		w_ji = w_ji + learning_rate*backward$dWji
		w_kj = w_kj + learning_rate*backward$dWkj
		
		i = i + 1

		# save current performance
		Error = backward$Error
		y_k = forward$y_k
		convergence = c(convergence, Error)
	}
	
	return(list('convergence' = convergence, 'x' = x, 'y_k' = y_k, 'Error' = Error, 'iterations' = i, 'w_kj' = w_kj, 'w_ji' = w_ji))
}
