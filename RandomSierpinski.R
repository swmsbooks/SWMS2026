##################################
# Construct Sierpinski by random functions
##################################

corners_T <- matrix(c(0, 0,  1, 0,  0.5, sqrt(3)/2), ncol = 2, byrow = TRUE)

# Three functions
S1 <- function(point) point/ 2 + corners_T[1, ]/2
S2 <- function(point) point/2 + corners_T[2, ]/2
S3 <- function(point) point/2 + corners_T[3, ]/2

###########################
# Function to generate Sierpiński Triangle
sierpinski <- function(iter = 1e4, prob = rep(1, 3)) 
{
  # Store points in this matrix
  points <- matrix(NA, nrow = iter, ncol = 2)
  
  # Start at (1, 0)
  # change this for Q3(d)
  point <- c(1, 0)
  
  # generating a sequence of functions (random!)
  sequence <- sample(1:3, iter, prob = prob, replace = TRUE)
  
  # Iterate the point (using for loop!)
  for (i in 1:iter) 
  {

    # Action S1, S2, or S3 based on 1, 2, or 3
    if(sequence[i] == 1)
    {
      point <- S1(point) 
    } else if(sequence[i] == 2)
    {
      point <- S2(point) 
    } else{
      point <- S3(point) 
    }
    points[i, ] <- point  # Store the new point
  }
  # return all points obtained
  return(points)
}

# Run the function to generate the fractal
triangle <- sierpinski(iter = 1e4)

# Plot to see the triangle!
plot(triangle, pch = 16)

