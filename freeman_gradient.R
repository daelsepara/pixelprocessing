freeman_gradient <- function(data_points,type) {
	
	size = length(data_points)
	gradient = array(0,size)
	running_sum = array(0,size)

	if (size > 2)
	{
		for (index in 1:size)
		{
			if (index == size)
			{
				gradient[index] = data_points[1] - data_points[index]
			}
			else
			{
				gradient[index] = data_points[index+1] - data_points[index]
			}
		}
		
		for (index in 1:size)
		{
			if (index > size-2)
			{
				if (index == size)
				{
					running_sum[index] = gradient[index]+gradient[1]+gradient[2]
				}
				else
				{
					running_sum[index] = gradient[index]+gradient[index+1]+gradient[1]
				}
			}
			else
			{
				running_sum[index] = gradient[index]+gradient[index+1]+gradient[index+2]
			}
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
