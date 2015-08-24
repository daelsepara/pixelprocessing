process_freeman <- function(data_points, start_x, start_y, threshold, type) {
	
	size = dim(data_points)
	size_x = size[1]; size_y = size[2]
	
	freeman = array(0.0,size)
	freeman_buffer = array(0.0,size_x*size_y)
	vertices = array(0.0,c(size_x*size_y,2))
	
	dx = c(-1, 0, 1, 1, 1, 0,-1,-1)
	dy = c(-1,-1,-1, 0, 1, 1, 1, 0)

	stop_condition = TRUE
	
	x = start_x; y = start_y; start_dir = 1
	
	index = 1
	
	while (stop_condition) {
	
		set_pix = FALSE
		
		for(dir_index in 1:8) {

			direction = (start_dir + dir_index - 2) %% 8 + 1
			next_x = x+dx[direction]; next_y = y+dy[direction]
			
			if (data_points[next_x,next_y]> threshold && freeman[next_x,next_y] == 0 && set_pix == FALSE || (next_x == start_x && next_y == start_y)) {
				
				set_pix = TRUE
				
				freeman[x, y] = direction; freeman_buffer[index] = direction
				
				vertices[index, ] = c(x,y)
				x = next_x; y = next_y
				
				index = index + 1
				
				if (direction > 4) {
					start_dir = 5
				} else {
					start_dir = 1
				}
				
				break
			}
		}
		
		if (!set_pix || (x == start_x && y == start_y)) {
			stop_condition = FALSE
		}
	}
	
	if (type == 'freeman') {
		return(freeman)
	}

	if (type == 'vector' && index > 1) {
		return(freeman_buffer[1:index-1])
	}
	
	if (type == 'vertices' && index > 1) {
		return(vertices[1:index-1,])
	}
	
	if (type == 'descriptor' && index > 1) {
		fd_ = complex(real = vertices[,1], imaginary = vertices[,2])
		return(fd_)
	}
}
