right_index <- function(i, size, n) {
	
	# compute index of nth neighbor to the right
	return ((i+n-1) %% size + 1)
	
}

left_index <- function(i, size, n) {
	
	# compute index of nth neighbor to the left
	return ((i+size-n-1) %% size + 1) 
}

freeman_gradient <- function(data_points,type) {
	
	size = length(data_points)
	gradient = array(0,size)
	running_sum = array(0,size)

	if (size > 2)
	{
		for (index in 1:size)
		{
			gradient[index] = data_points[right_index(index,size,1)] - data_points[index]
		}
		
		for (index in 1:size)
		{
			running_sum[index] = gradient[index]+gradient[right_index(index,size,1)]+gradient[right_index(index,size,2)]
		}
	}
	
	if (type == 'gradient')
	{
		return(gradient)
	}
	
	if (type == 'sum')
	{
		return(running_sum)
	}
}
