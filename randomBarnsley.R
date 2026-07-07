# Generating Barnsley Fern

# all matrices for different tasks
stem <- matrix(c(0, 0, 0, .16), ncol = 2, nrow = 2)
small_leaf <- matrix(c(0.85, -0.04, .04, .85), ncol = 2, nrow = 2)
large_left_leaf <- matrix(c(0.2, 0.23, -0.26, .22), ncol = 2, nrow = 2)
large_right_leaf <- matrix(c(-0.15, 0.26, 0.28, .24), ncol = 2, nrow = 2)

# the various functions
S1 <- function(vec) stem %*% vec + c(0,0)
S2 <- function(vec) small_leaf %*% vec + c(0, 1.6)
S3 <- function(vec) large_left_leaf %*% vec + c(0, 1.6)
S4 <- function(vec) large_right_leaf %*% vec + c(0, .44)

# Function that creates the leaf
barnsley <- function(iter = 5)
{
  out <- matrix(0, ncol = 2, nrow = iter)
  # start from origin
  out[1, ] <- c(0,0)

  # generating a sequence of functions
  sequence <- sample(...) #1:4, iter, prob = c(0.01, 0.85, .07,.07), replace = TRUE)
  
  for(i in 2:iter)
  {
    # choose part of leaf according to sequence
    if(sequence[i] == 1)
    {
      out[i, ] <- S1(out[i-1, ]) 
    } else if(sequence[i] == 2)
    {
      out[i, ] <- S2(out[i-1, ]) 
    } else if(sequence[i] == 3){
      out[i, ] <- S3(out[i-1, ]) 
    } else{
      out[i, ] <- S4(out[i-1, ]) 
    }
  }
  return(list(out, sequence))
}

fern1 <- barnsley(iter = 5)
fern2 <- barnsley(iter = 100)
fern3 <- barnsley(iter = 1000)
fern4 <- barnsley(iter = 10000)

par(mfrow = c(2,2))
plot(fern1[[1]], pch = 20, col = fern1[[2]] + 1, xlab = "Barnsley Fern x-coordinate", ylab = "Barnsley Fern-ycoordinate",  main = "Barnsley Fern Iteration 5")
plot(fern2[[1]], pch = 20, col = fern2[[2]] + 1, xlab = "Barnsley Fern x-coordinate", ylab = "Barnsley Fern-ycoordinate",  main = "Barnsley Fern Iteration 100")
plot(fern3[[1]], pch = 20, col = fern3[[2]] + 1, xlab = "Barnsley Fern x-coordinate", ylab = "Barnsley Fern-ycoordinate",  main = "Barnsley Fern Iteration 1000")
plot(fern4[[1]], pch = 20, col = fern4[[2]] + 1, xlab = "Barnsley Fern x-coordinate", ylab = "Barnsley Fern-ycoordinate",  main = "Barnsley Fern Iteration 10000")


##############################################

# Using modified maps

# set value of a
 a = 1


# all matrices for different tasks
stem <- matrix(c(0, 0, 0, .16), ncol = 2, nrow = 2)
small_leaf <- matrix(c(0.85, -0.04, .04, .85), ncol = 2, nrow = 2)
large_left_leaf <- matrix(c(0.2, 0.23, -0.26, .22), ncol = 2, nrow = 2)
large_right_leaf <- matrix(c(-0.15, 0.26, 0.28, .24), ncol = 2, nrow = 2)

# the various functions
mS1 <- S1
mS2 <- S2
mS3 <- ??
mS4 <- ??

# Function that creates the leaf
modified_barnsley <- function(iter = 1e4)
{
  out <- matrix(0, ncol = 2, nrow = iter)
  # start from origin
  out[1, ] <- c(0,0)
  
  # generating a sequence of functions
  sequence <- sample(...) #1:4, iter, prob = c(0.01, 0.85, .07,.07), replace = TRUE)
  
  for(i in 2:iter)
  {
    # choose part of leaf according to sequence
    if(sequence[i] == 1)
    {
      out[i, ] <- mS1(out[i-1, ]) 
    } else if(sequence[i] == 2)
    {
      out[i, ] <- mS2(out[i-1, ]) 
    } else if(sequence[i] == 3){
      out[i, ] <- mS3(out[i-1, ]) 
    } else{
      out[i, ] <- mS4(out[i-1, ]) 
    }
  }
  return(list(out, sequence))
}

mfern4 <- modified_barnsley(iter = 10000)

par(mfrow = c(1,2))
plot(fern4[[1]], pch = 16, col = fern4[[2]] + 1)
plot(mfern4[[1]], pch = 16, col = mfern4[[2]] + 1)
