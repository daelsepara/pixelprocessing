cpq <- function(f_, p_, q_) {
  size = dim(f_)
  xyp = outer(array(1:size[1])*complex(imaginary=1),array(1:size[2]),FUN='+')
  return(sum(xyp^p_*Conj(xyp)^q_*f_))
}

flusser <- function(f_) {
  
  phi_f = array(0,4)
  
  phi_f[1] = cpq(f_,1,1)
  phi_f[2] = cpq(f_,2,1)*cpq(f_,1,2)
  phi_f[3] = cpq(f_,2,0)*(cpq(f_,1,2)^2)
  phi_f[4] = cpq(f_,3,0)*(cpq(f_,1,2)^3)
  
  return(phi_f)
}

flusser_hu <- function(f_) {
  
  phi_ = array(0,7)
  phi_f = flusser(f_)
  
  phi_[1] = phi_f[1]
  phi_[2] = Mod(phi_f[3])^2/phi_f[2]^2
  phi_[3] = Mod(phi_f[4])^2/phi_f[2]^3
  phi_[4] = phi_f[2]
  phi_[5] = Re(phi_f[4])
  phi_[6] = Re(phi_f[3])
  phi_[7] = Im(phi_f[4])
  
  return(abs(phi_))
}
