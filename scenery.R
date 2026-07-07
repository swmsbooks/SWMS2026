###########################
# sierpinski triangle
###########################
# The three corners
corners_T <- matrix(c(0, 0,  1, 0,  0.5, sqrt(3)/2), ncol = 2, byrow = TRUE)

# Three functions
S1 <- function(point) point/ 2 + corners_T[1, ]/2  # (x + P)/2
S2 <- function(point) point/2 + corners_T[2, ]/2   # (x + Q)/2
S3 <- function(point) point/2 + corners_T[3, ]/2  # (x + R)/2
####################################################

# Right angle sierpinski triangle

rcorners_T <- matrix(c(0, 0,  1, 0,  0, 1), ncol = 2, byrow = TRUE)
# Three functions
S1r <- function(point) point/ 2 + rcorners_T[1, ]/2  # (x + P)/2
S2r <- function(point) point/2 + rcorners_T[2, ]/2   # (x + Q)/2
S3r <- function(point) point/2 + rcorners_T[3, ]/2  # (x + R)/2
##########################

### Making Sierpinski triangle
sierpinski <- function(iter = 1e4) 
{
  # Store points in this matrix
  points <- matrix(NA, nrow = iter, ncol = 2)
  
  # Start at (1, 0)
  # change this for Q3(d)
  point <- c(1, 0)
  
  # generating a sequence of functions (random!)
  sequence <- sample(1:3, iter, replace = TRUE)
  
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

right_sierpinski <- function(iter = 1e4) 
{
  # Store points in this matrix
  points <- matrix(NA, nrow = iter, ncol = 2)
  
  # Start at (1, 0)
  # change this for Q3(d)
  point <- c(1, 0)
  
  # generating a sequence of functions (random!)
  sequence <- sample(1:3, iter, replace = TRUE)
  
  # Iterate the point (using for loop!)
  for (i in 1:iter) 
  {
    # Action S1, S2, or S3 based on 1, 2, or 3
    if(sequence[i] == 1)
    {
      point <- S1r(point) 
    } else if(sequence[i] == 2)
    {
      point <- S2r(point) 
    } else{
      point <- S3r(point) 
    }
    points[i, ] <- point  # Store the new point
  }
  # return all points obtained
  return(points)
}

equi_tri <- sierpinski(1e4) # Equilateral Triangle
right_tri <- right_sierpinski(1e4) # Right angle triangle

# Transforming the triangle
# x is a vector
# h > 0
# move is a vector
# theta is in radians!!!
tri_transform <- function(x, h = 1, move = c(1/2, 1/(2*sqrt(3))), theta = 0)
{
  # write your function here
  A <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol = 2, nrow = 2)
  Lx <- h*A%*% (x - c(1/2, 1/(2*sqrt(3))) )
  u <- move
  out <- Lx + u
}

# Transforming the right angle triangle
# x is a vector
# h > 0
# move is a vector
# theta is in radians!!!
right_tri_transform <- function(x, h = 1, move = c(1/2, 1/(2*sqrt(3))), theta = 0)
{
  # write your function here
  A <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol = 2, nrow = 2)
  Lx <- h*A%*% (x - c(1/3, 1/3) )
  u <- move
  out <- Lx + u
}

################################
#   snow
mat1 <- matrix(c(cos(pi/3), sin(pi/3), -sin(pi/3), cos(pi/3)), ncol = 2, nrow = 2)
mat2 <- matrix(c(cos(-pi/3), sin(-pi/3), -sin(-pi/3), cos(-pi/3)), ncol = 2, nrow = 2)
snow_S1 <- function(point) point/3
snow_S2 <- function(point) mat1 %*% point/3  + c(1/3,0)
snow_S3 <- function(point) mat2 %*% point/3  + c(1/2,sqrt(3)/6)
snow_S4 <- function(point) point/3 + c(2/3, 0)

snow_flake <- function(iter = 1e4) 
{
  # Store points in this matrix
  points <- matrix(NA, nrow = iter, ncol = 2)
  
  # Start at (1, 0)
  # change this for Q3(d)
  point <- c(1, 0)
  
  # generating a sequence of functions (random!)
  sequence <- sample(1:4, iter, replace = TRUE)
  
  # Iterate the point (using for loop!)
  for (i in 1:iter) 
  {
    # Action S1, S2, or S3 based on 1, 2, or 3
    if(sequence[i] == 1)
    {
      point <- snow_S1(point) 
    } else if(sequence[i] == 2)
    {
      point <- snow_S2(point) 
    } else if(sequence[i] == 3)
    {
      point <- snow_S3(point) 
    } else{
      point <- snow_S4(point) 
    }
    points[i, ] <- point  # Store the new point
  }
  # return all points obtained
  return(points)
}

water <- snow_flake(1e3)

# Function to transform this
water_trans <- function(x, h = 1, move = c(0,0), theta = 0)
{
  # write your function here
  A <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol = 2, nrow = 2)
  Lx <- h*A%*% (x - c(.5,0))
  u <- move
  out <- Lx + u
}



#######################################
# Fern
# Generating Barnsley Fern

# all matrices for different tasks
stem <- matrix(c(0, 0, 0, .16), ncol = 2, nrow = 2)
small_leaf <- matrix(c(0.85, -0.04, .04, .85), ncol = 2, nrow = 2)
large_left_leaf <- matrix(c(0.2, 0.23, -0.26, .22), ncol = 2, nrow = 2)
large_right_leaf <- matrix(c(-0.15, 0.26, 0.28, .24), ncol = 2, nrow = 2)

# the various functions
fern_S1 <- function(vec) stem %*% vec + c(0,0)
fern_S2 <- function(vec) small_leaf %*% vec + c(0, 1.6)
fern_S3 <- function(vec) large_left_leaf %*% vec + c(0, 1.6)
fern_S4 <- function(vec) large_right_leaf %*% vec + c(0, .44)

# Function that creates the leaf
barnsley <- function(iter = 1e4)
{
  out <- matrix(0, ncol = 2, nrow = iter)
  # start from origin
  out[1, ] <- c(0,0)
  
  # generating a sequence of functions
  sequence <- sample(1:4, iter, prob = c(0.01, 0.85, 0.07, 0.07), replace = TRUE)
  
  for(i in 2:iter)
  {
    # choose part of leaf according to sequence
    if(sequence[i] == 1)
    {
      out[i, ] <- fern_S1(out[i-1, ]) 
    } else if(sequence[i] == 2)
    {
      out[i, ] <- fern_S2(out[i-1, ]) 
    } else if(sequence[i] == 3){
      out[i, ] <- fern_S3(out[i-1, ]) 
    } else{
      out[i, ] <- fern_S4(out[i-1, ]) 
    }
  }
  return(list(out, sequence))
}

fern <- barnsley(5e2)[[1]]

fern_trans <- function(x, h = 1, move = c(0,0), theta = 0)
{
  # write your function here
  A <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol = 2, nrow = 2)
  Lx <- h*A%*% (x - c(.8,6))  # c(.8, 6) is roughly the mean of a fern
  u <- move
  out <- Lx + u
}


#########################
# Need points on a circle for the tree

##### points on the circle
points_circle <- function(center = c(0,0), r = .15)
{
  count <- 0  
  while(count < 1)
  {
    point <- runif(2, min = center[1] - r, max = center[2] + r)
    if( norm(point - center, "2") < r)
    {
      return(point)
    }
  }
}
#########################

# Cantor set

CS1 <- function(point) point/3
CS2 <- function(point) point/3 + c(2/3, 0)

# Function that creates the leaf
cantorSet <- function(iter = 1e4)
{
  out <- matrix(0, ncol = 2, nrow = iter)
  # start from origin
  out[1, ] <- c(0,0)
  
  # generating a sequence of functions
  sequence <- sample(1:2, iter, replace = TRUE)
  
  for(i in 2:iter)
  {
    # choose part of leaf according to sequence
    if(sequence[i] == 1)
    {
      out[i, ] <- CS1(out[i-1, ]) 
    } else
    {
      out[i, ] <- CS2(out[i-1, ]) 
    } 
  }
  return(out)
}

cantor <- cantorSet(1e3)

cantor_trans <- function(x, h = 1, move = c(0,0), theta = 0)
{
  # write your function here
  A <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol = 2, nrow = 2)
  Lx <- h*A%*% (x - c(.5,0))  # c(.8, 6) is roughly the mean of a fern
  u <- move
  out <- Lx + u
}
#########################

# Starting to make the scenery now!

## Making the mountains
par(mfrow = c(1,1))
m1 <- t(apply(equi_tri, 1, tri_transform, h = .2, move = c(.08,.9)))


### Now add your own mountains, and other things to this code!
# Start making plot
par(bg = "beige")
plot(m1, xlim = c(0, 1), ylim = c(0, 1.1), type = "n", asp = 1,
     xlab = "", ylab = "", main = "Fractal Land")
points(m1[,1], m1[,2], col = "chocolate4", cex = .2)
