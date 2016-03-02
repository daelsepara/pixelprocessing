create_training_data <- function(n = 100) {

  require(jpeg)
  
  patterns_per_sample = n / 10
  
  training_set = array(0, c(n, 784))
  training_output = array(0, c(n, 1))
  
  for (ss in 0:9) {
    
    data_ = readJPEG(paste0('mnist_train', ss, '.jpg'))
    reshape_ = array(0, c(length(data_)/784, 784))
    
    k = 1
    
    for(j in 1:(dim(data_)[1]/28)) {
      for(i in 1:(dim(data_)[2]/28)) {
        reshape_[k, ] = array(data_[((j-1)*28+1):((j-1)*28+28),((i-1)*28+1):((i-1)*28+28)], 784)
        k = k + 1
      }
    }
    
    reshape_

    i0 = reshape_ <= 0.5
    i1 = reshape_ > 0.5
    
    reshape_[i0] = 0
    reshape_[i1] = 1
    
    training_set[(ss*patterns_per_sample+1):(ss*patterns_per_sample+patterns_per_sample), ] = reshape_[1:patterns_per_sample, ]
    training_output[(ss*patterns_per_sample+1):(ss*patterns_per_sample+patterns_per_sample)] = ss + 1
  }
  
  return(list('training_set' = training_set, 'training_output' = training_output))
  
}