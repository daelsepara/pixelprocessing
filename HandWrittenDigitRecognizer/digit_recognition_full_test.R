test_digit_recognizer <- function(large_set, netWji, netWkj) {
	
	full_test = array(0, c(10, 10))
	
	for (dd in 0:9) {

		for (di in 0:9) {

			recog_perf = 0
			
			test_sets = 11:1000
			
			for (tt in test_sets) {
			
				oo = di*1000 + tt
				
				recog_perf = recog_perf + as.double(nnet_forward(large_set[oo, ], array(netWji[,, dd+1], c(784, 100)), array(netWkj[,,dd+1], c(100, 1)))$y_k)
			}
			
			recog_perf = recog_perf / length(test_sets)
			
			full_test[dd+1, di + 1] = recog_perf

		}
	}
	
	return(full_test)
}