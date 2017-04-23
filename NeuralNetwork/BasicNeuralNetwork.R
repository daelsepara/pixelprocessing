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
nnet_forward <- function(x, w_ji, w_kj) {
  
  # comput outputs in hidden layer with bias = 1.0
  if (is.vector(x)) {
    z_j = as.vector(h_func(w_ji %*% x + 1.0))
  } else {
    z_j = t(h_func(w_ji %*% t(x) + 1.0))
  }
  
  # compute output with bias = 1.0
  y_k = as.vector(h_func(z_j %*% w_kj + 1.0))
  
  return(list('y_k' = y_k, 'z_j' = z_j))
}

# backward propagation
nnet_backprop <- function (y_k, z_j, x, t_k, w_kj) {
  
  error_k = (t_k - y_k)
  delta = error_k * h_funcd(y_k)
  
  # compute weight updates based on Error and propagate backwards through the network
  dWkj = as.vector(delta %*% z_j)
  dWji = t(outer(delta,w_kj) * h_funcd(z_j)) %*% x
  
  return(list('dWkj' = dWkj, 'dWji' = dWji, 'Error' = sum(error_k^2)/2))
}

# network training
nnet_train <-function(maxiter = 1000000, learning_rate = 0.1, tol = 10^(-3), training_set = array(c(0, 0, 1, 1, 0, 0, 0, 1), c(4, 2)) , output = c(0, 1, 1, 0), hidden_units = 4, min_max = 1, isGaussian = FALSE) {
  
  # determine network dimensions from user input
  j = hidden_units
  inputs = ncol(training_set)
  
  # intialize interconnection weights with random values (-min_max, min_max) or Gaussian (mean = 0, sd = min_max)
  if (!isGaussian) {
    w_ji = array(runif(n = j * inputs, min = -min_max, max = min_max), c(j, inputs))
    w_kj = array(runif(n = j, min = -min_max, max = min_max), j)
  } else {
    w_ji = array(rnorm(n = j * inputs, mean = 0, sd = min_max), c(j, inputs))
    w_kj = array(rnorm(n = j, mean = 0, sd = min_max), j)
  }
  
  # begin at a high error value
  Error = 1.0
  i = 0
  y_p = numeric(0)
  
  # training loop (batch processing)
  while (i < maxiter && Error > tol) {
    
    # perform both forward and backpropagation on all patterns each iteration
    forward = nnet_forward(training_set, w_ji, w_kj)
    backward = nnet_backprop(forward$y_k, forward$z_j, training_set, output, w_kj)
    
    # update weights
    w_ji = w_ji + learning_rate * backward$dWji
    w_kj = w_kj + learning_rate * backward$dWkj
    
    # save current performance
    Error = backward$Error
    y_p = forward$y_k
    
    i = i + 1
    
    if (i %% 1000 == 0) {
      print(paste('iteration = ', i, ' Error = ', Error))
    }
  }
  
  return(list('y_p' = y_p, 'Error' = Error, 'iterations' = i, 'w_kj' = w_kj, 'w_ji' = w_ji))
}
