digit_heatmap <- function(large_set, w_ji, w_kj) {
  
  dig_heatmap = array(0, dim(large_set)[1])
  
  for (di in 0:9) {
    for (tt in 1:1000) {
      oo = di*1000 + tt
      dig_heatmap[oo] = as.double(nnet_forward(large_set[oo, ], w_ji, w_kj)$y_k)
    }
  }
  
  return(dig_heatmap)
}
