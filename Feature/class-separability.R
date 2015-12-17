# compute between class separability
Sbetween <- function(x) {
  
  size = dim(x)
  
  # number of features
  f = size[1]
  
  # number of samples
  i = size[2]
  
  # number of classes
  c_ = size[3]

  Sb = array(0, c(f, f))

  # global mean
  u0 = array(0, f)
  
  for (m in 1:f) {
    u0[m] = mean(x[m, ,])
  }
  
  # compute in-between class separability matrix
  for (k in 1:c_) {
    
    u_ = array(0, f)
    
    for (m in 1:f) {
      u_[m] = mean(x[m,,k])
    }
    
    diff = u_ - u0
    
    Sb = Sb + diff %*% t(diff)/c_
  }
  
  return(Sb)
}

# compute within-class separability
Swithin <- function(x) {
  
  size = dim(x)
  
  # number of features
  f = size[1]
  
  # number of samples
  i = size[2]
  
  # number of classes
  c_ = size[3]
  
  Sw = array(0, c(f, f))
  
  # compute within class separability matrix
  for (k in 1:c_) {
    
    u_ = array(0, f)
    
    for (m in 1:f) {
      u_[m] = mean(x[m,,k])
    }
    
    for (j in 1:i) {
      diff = x[, j, k] - u_
      Sw = Sw + diff %*% t(diff)/i
    }
  }
  
  return(Sw)
}

Smixture <- function(x) {
  
  return(Sbetween(x) + Swithin(x))
  
}

SJ1 <- function(x) {
  
  require(psych)
  
  return(tr(Smixture(x))/tr(Swithin(x)))
}

SJ2 <- function(x) {
  
  return(det(Smixture(x))/det(Swithin(x)))
}

SJ3 <- function(x) {
  
  require(psych)
  
  return(tr(solve(Swithin(x))%*%Smixture(x)))
}

