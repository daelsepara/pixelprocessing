# sigmoid activation function
h_func <- function(x) {
	return(1/(1+exp(-x)))
}

# 1st-derivative of activation function
h_funcd <- function(x) {
	h = h_func(x)
	return(x*(1-x))
}

# boolean gate operation (defaults to XOR operation)
boolean_gate <- function(output = c(0, 1, 1, 0)) {

	booldata = array(0, c(4, 2))
	booldata[1, ] = c(0, 0)
	booldata[2, ] = c(0, 1)
	booldata[3, ] = c(1, 0)
	booldata[4, ] = c(1, 1)
	
	# XOR operation
	return(list('patterns' = booldata, 'output' = output))
}

# feed-forward operation
nnet_forward <- function(x, w_ji, w_kj) {
	
	# activation function with bias = 1
	z_j = h_func(x %*% w_ji + 1)
	y_k = h_func(z_j %*% w_kj + 1)
	
	return(list('y_k' = y_k, 'z_j' = z_j))
}

# backward propagation
nnet_backprop <- function (y_k, z_j, x, t_k, w_kj) {
	
	error_k = t_k - y_k
	delta_k = error_k*h_funcd(y_k)

	error_j = delta_k %*% (t(w_kj))
	delta_j = error_j * h_funcd(z_j)

	# compute weight updates
	dWkj = t(z_j) %*% (delta_k)
	dWji = t(x) %*% (delta_j)

	Error = mean(abs(error_k))

	return(list('dWkj' = dWkj, 'dWji' = dWji, 'Error' = Error))
}

nnet_train <-function(maxiter = 100000, learning_rate = 0.1, tol = 10^(-3), bool_op = c(0, 1, 1, 0)) {
	
	boolean_op = boolean_gate(bool_op)
	
	x = boolean_op$patterns
	t_k = boolean_op$output
	
	ii = dim(x)
	Error = 10000;
	
	w_ji = array(runif(n = ii[1]*ii[2], min = -1, max = 1), c(ii[2], ii[1]))
	w_kj = array(runif(n = ii[1], min = -1, max = 1), c(ii[1], 1))
	
	i = 0
	
	output = array(0, length(t_k))
	
	while (i < maxiter && Error > tol) {
		
		forward = nnet_forward(x, w_ji, w_kj)
		backward = nnet_backprop(forward$y_k, forward$z_j, x, t_k, w_kj)
		
		Error = backward$Error
		output = forward$y_k
		
		w_ji = w_ji + learning_rate*backward$dWji
		w_kj = w_kj + learning_rate*backward$dWkj
		
		i = i + 1
	}
	
	return(list('x' = x, 'y_k' = output, 'Error' = Error, 'iterations' = i, 'w_kj' = w_kj, 'w_ji' = w_ji))
}
