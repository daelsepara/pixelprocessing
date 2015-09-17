lbp <- function(f) {

	# weights of neighboring pixels
	#
	#   1|  2|  4
	# ------------
	# 128|  0|  8
	# ------------
	#  64| 32| 16
	w_ = c(1, 128, 64, 2, 0, 32, 4, 8, 16)
	
	lbp = array(0,dim(f))
	
	size = dim(f)
	
	for(y in 2:(size[1]-1)) {
		for(x in 2:(size[2]-1)) {
		  
			# apply threshold
			gn = as.vector(f[(y-1):(y+1),(x-1):(x+1)])
			i = which(gn >= f[y,x])
			
			# compute LBP based on weights
			lbp[y,x] = sum(w_[i])
		}
	}
	
	return(lbp[2:(size[1]-1),2:(size[2]-1)])
}

bit_transitions <- function(i_) {

	dir_ = bitwAnd(i_, 1)
	
	transitions = 0
	
	# count number of bit transitions
	for (i in 1:7) {
		
		dir_n = bitwAnd(bitwShiftR(i_, i), 1)

		# check for bit-transitions 0 -> 1 or 1 -> 0
		if (dir_n != dir_) {
			transitions = transitions + 1
			dir_ = dir_n
		}
	}
	
	return(transitions)
}

u_lbp <- function(f) {
	
	size = dim(f)
	
	# weights of neighboring pixels
	#
	#   1|  2|  4
	# ------------
	# 128|  0|  8
	# ------------
	#  64| 32| 16
	w_ = c(1, 128, 64, 2, 0, 32, 4, 8, 16)
	
	U_ = array(0,size)
	
	for(y in 2:(size[1]-1)) {
		for(x in 2:(size[2]-1)) {
		
		  gn = as.vector(f[(y-1):(y+1),(x-1):(x+1)])
		  i = which(gn >= f[y,x])
		  
		  # compute LBP based on weights
		  lbp_ = sum(w_[i])
			
		  # compute LBP_P,R^RUI2
			if (bit_transitions(lbp_) <= 2) {
		    U_[y,x] = length(i)
			} else {
			  U_[y,x]= 9
			}
		}
	}
	
	return(U_[2:(size[1]-1),2:(size[2]-1)])
}

u_npr <- function(f) {
  
  size = dim(f)
  unpr = array(0, size)
  
  s_ = array(0, 8)
  
  for (y in 2:(size[1]-1)) {
    for (x in 2:(size[2]-1)) {
      
      # count number of bit transitions
      s_[1] = f[y-1, x-1]
      s_[2] = f[y-1, x]
      s_[3] = f[y-1, x+1]
      s_[4] = f[y, x+1]
      s_[5] = f[y+1, x+1]
      s_[6] = f[y+1, x]
      s_[7] = f[y+1, x-1]
      s_[8] = f[y, x-1]
      
      s_ = s_ - f[y,x]
      
      s_[which(s_ >= 0)] <- 1
      s_[which(s_  < 0)] <- 0

      ss = abs(s_[8] - s_[1])
      
      for(ii in 2:8) {
        ss = ss + abs(s_[ii] - s_[ii-1])
      }
      
      # compute LBP_P,R^RUI2
      if(ss <= 2) {
        unpr[y,x] = sum(s_)
      } else {
        unpr[y,x] = 9
      }
    }
  }
  
  return(unpr[2:(size[1]-1),2:(size[2]-1)])
}

var_pr <- function(f) {
  
  size = dim(f)
  var_ = array(0,size)
  
  for(y in 2:(size[1]-1)) {
    for(x in 2:(size[2]-1)) {
      
      gp = as.vector(f[(y-1):(y+1),(x-1):(x+1)])
      
      # remove center pixel
      gp = gp[-5]
      
      u_ = mean(gp)

      # compute VAR_P,R      
      var_[y,x] = mean((gp-u_)^2)
    }
  }
  
  return(var_[2:(size[1]-1),2:(size[2]-1)])

}

lbp_hist <- function(ulbp_) {
  
  hist_ = array(0, 10)

  # Determine LBP_P,R^RUI2 histogram
  for (i in 0:9) {
    hist_[i+1] = length(which(ulbp_ == i))
  }
  
  return(hist_)
  
}

lbp_contrast <- function(f) {
  
  lbp_c = array(0,dim(f))
  
  size = dim(f)
  
  for(y in 2:(size[1]-1)) {
    for(x in 2:(size[2]-1)) {
      
      gn = as.vector(f[(y-1):(y+1),(x-1):(x+1)])
      
      # remove center pixel
      gn = gn[-5]
      
      i = which(gn >= f[y,x])
      j = !(1:8 %in% i)
      
      # compute LBP contrast
      c_ = 0
      
      if (length(i) > 0) {
        c_ = c_ + sum(gn[i])/length(i)
      }
      
      if (length(j) > 0) {
        c_ = c_ - sum(gn[j])/length(j)
      }
      
      lbp_c[y,x] = c_
    }
  }
  
  return(lbp_c[2:(size[1]-1),2:(size[2]-1)])
}