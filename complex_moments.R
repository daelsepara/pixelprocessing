# simplified computation of Flusser-Suk-Zitova rotational invariant complex moments
# adapted from: http://homepages.inf.ed.ac.uk/rbf/CVonline/LOCAL_COPIES/FISHER/MOMINV/

cpq <- function(f_, p_, q_) {
  size = dim(f_)
  
  # compute for centralized complex moment
  x_ = array(1:size[2]);
  y_ = array(1:size[1]);
  xyp = outer((y_-mean(y_))*complex(imaginary=1),x_-mean(x_),FUN='+')
  
  return(sum(xyp^p_*Conj(xyp)^q_*f_))
}

flusser <- function(f_) {
  
  phi_f = array(0,6)
  
  A = sum(f_)
  s11 = cpq(f_,1,1)/A^2
  s20 = cpq(f_,2,0)/A^2
  s21 = cpq(f_,2,1)/A^2.5
  s12 = cpq(f_,1,2)/A^2.5
  s30 = cpq(f_,3,0)/A^2.5
  
  phi_f[1] = Re(s11)
  phi_f[2] = Re(1000*s21*s12)
  phi_f[3] = 10000*Re(s20*s12*s12)
  phi_f[4] = 10000*Im(s20*s12*s12)
  phi_f[5] = 1000000*Re(s30*s12*s12*s12)
  phi_f[6] = 1000000*Im(s30*s12*s12*s12)
  
  return(phi_f)
}
