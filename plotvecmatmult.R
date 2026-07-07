# SWMS 2026 day2 worksheet-2 R codes
## load the following library to use viridis colours in plots
library(viridisLite)

## generating viridis colour palatte of size 14
v_col <- viridis(n = 14, option = "H")
####################################################################################

## Q2 ## visualize the effect of multiplication of the above vectors by the matrix
## first we compute the vector and matrix multiplication

# Define the vector 
a <- c(3, 4)

## writing matrix A
A <- matrix(c(1, -2, 3, 4), nrow=2, ncol=2)
A

## multiplying matrix A by a (above)
a_transformed <-  A%*%a #Aa
a_transformed

# Vectors are stored as rows
#so we convert the column vector to a row vector using t(),i.e. transpose of a vector or matirx
a_transformed <- t(a_transformed) 
a_transformed

# we shall now plot
par(mfrow = c(1, 2))

#make  an empty plot with required axes
plot(xlim <- c(-1,16), ylim <- c(0,12), type = "n", xlab = "X1", ylab = "X2",
     main = "Plot vector a") 
grid() #add grid lines

# Add vector and point for a vector
arrows(x0 = 0, y0 = 0, x1 =a[1], y1 = a[2], col = v_col[1])
points(a[1], a[2], pch = 19, cex = 1, col = v_col[2])
text(2.5, 2.5, "a", col = v_col[2])

# Plot the transformed vector into another plot window
plot(xlim <- c(-1,16), ylim <- c(0,12), type = "n", xlab = "X1", ylab = "X2",
     main = "Plot vector Aa")
grid() #empty plot with grid

# vectors(a, labels="a", pos.lab=2.9, frac.lab=.5, col=v_col[1])
arrows(x0 = 0, y0 = 0, x1 =a_transformed[1], y1 = a_transformed[2], col = v_col[1])
points(a_transformed[1], a_transformed[2], pch = 19, cex = 1, col = v_col[2])
text(2.5, 2.5, "Aa", col = v_col[2])

# Note that both the length of the vector and the direction changes
# similarly do it for other vectors

