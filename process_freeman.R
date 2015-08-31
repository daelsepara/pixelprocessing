process_freeman <- function(data_points, start_y, start_x, threshold, type) {
	
	size = dim(data_points)
	size_x = size[2]; size_y = size[1]
	
	freeman = array(0.0,size)
	freeman_buffer = array(0.0,size_x*size_y)
	vertices = array(0.0,c(size_x*size_y,2))
	
	dy = c(-1, 0, 1, 1, 1, 0,-1,-1)
	dx = c(-1,-1,-1, 0, 1, 1, 1, 0)
	dirs = c(1,8,7,6,5,4,3,2)
	
	stop_condition = TRUE
	
	x = start_x; y = start_y; start_dir = 1
	
	index = 1
	
	while (stop_condition) {
	
		set_pix = FALSE
		
		for(dir_index in 1:8) {

			direction = (start_dir + dir_index - 2) %% 8 + 1
			next_x = x+dx[direction]; next_y = y+dy[direction]
			
			if (data_points[next_y,next_x] > threshold && freeman[next_y,next_x] == 0 && set_pix == FALSE || (next_x == start_x && next_y == start_y)) {
				
				set_pix = TRUE
				
				freeman[y, x] = dirs[direction]; freeman_buffer[index] = dirs[direction]
				
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
	
	# Freeman codes image
	if (type == 'freeman') {
		return(freeman)
	}

	# Freeman codes vector
	if (type == 'vector' && index > 1) {
		return(freeman_buffer[1:index-1])
	}
	
	# vertices
	if (type == 'vertices' && index > 1) {
		return(vertices[1:index-1,])
	}
	
	# Fourier descriptor
	if (type == 'descriptor' && index > 1) {
		fd_ = complex(real = vertices[,1], imaginary = vertices[,2])
		return(fd_)
	}
	
	# binary image of shape
	if (type == 'boundary' && index > 1) {
		freeman[vertices[,2:1]]<-1
		return(freeman)
	}
}
