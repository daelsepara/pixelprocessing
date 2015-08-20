process_freeman <- function(data_points, start_x, start_y, threshold, type) {
	
	size = dim(data_points)
	size_x = size[1]
	size_y = size[2]
	
	freeman = array(0.0,c(size_x,size_y))
	freeman_buffer = array(0.0,size_x*size_y)
	vertices = array(0.0,c(size_x*size_y,2))
	
	dx = array(0, 8);
	dy = array(0, 8);
	
	dx[1] = -1; dy[1] = -1;
	dx[2] =  0; dy[2] = -1;
	dx[3] =  1; dy[3] = -1;
	dx[4] =  1; dy[4] =  0;
	dx[5] =  1; dy[5] =  1;
	dx[6] =  0; dy[6] =  1;
	dx[7] = -1; dy[7] =  1;
	dx[8] = -1; dy[8] =  0;

	stop_condition = TRUE
	
	x = start_x
	y = start_y
	index = 1
	pos = 1;
	start_dir = 1;
	
	while (stop_condition) {
	
		next_x = 0
		next_y = 0
		
		set_pix = FALSE
		
		for(dir_index in 1:8)
		{
			direction = start_dir + dir_index - 1;
			
			if (direction > 8)
			{
				direction = direction - 8
			}
			
			if (data_points[x+dx[direction],y+dy[direction]]> threshold && freeman[x+dx[direction],y+dy[direction]] == 0  && set_pix == FALSE || (x+dx[direction] == start_x && y+dy[direction] == start_y))
			{
				set_pix = TRUE

				freeman[x, y] = direction
				freeman_buffer[index] = direction
				vertices[index, 1] = x
				vertices[index, 2] = y
				
				if (direction > 4)
				{
					start_dir = 5
				}
				else
				{
					start_dir = 1
				}
				
				next_x = x+dx[direction]
				next_y = y+dy[direction]
				
				index = index + 1
				
				break;
			}
		}
		
		if (set_pix) {
			x = next_x
			y = next_y
		} else {
			stop_condition = FALSE
		}

		if (x == start_x && y == start_y) {
			stop_condition = FALSE
		}
	}
	
	if (type == 'freeman')
	{
		return(freeman)
	}

	if (type == 'vector')
	{
		if (index > 1)
		{
			return(freeman_buffer[1:index-1])
		}
	}
	
	if (type == 'vertices')
	{
	  if (index > 1)
	  {
	    return(vertices[1:index-1,])
	  }
	}
}
