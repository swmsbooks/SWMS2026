# SWMS 2026 day2 worksheet-2 R codes

###load the following library to use viridis colours in plots
library(viridisLite)

## generating viridis colour palatte of size 14
v_col <- viridis(n = 14, option = "H")
####################################################################################

## Q3## visualize the effect of multiplication on the sets by the matrix
## ellipse
# Let's generate some points first on the x1
x1 <- seq(-3, 3, length = 1000)

# We'll now generate the points for x2 using (x1^2)/9+(x2^2)/16 = 1
# Here for each x1, x2 can take positive and negative values.
x2_pos = 4* sqrt(1-x1^2/9)  ### x2 = 4(1-x1^(2)/9)^(0.5)
x2_neg = - 4* sqrt(1-x1^2/9)  ### x2 = -4(1-x1^(2)/9)^(0.5)

# visualize this shape, i.e., Ellipse
par(mfrow =c(1, 2)) 
plot(xlim <- c(-7, 7),  ylim <- c(-7, 7), type = "n", xlab = "X1", ylab = "X2",
     main = "Plot the set") 
grid()  ### adding grids
points(x1, x2_pos, col = v_col[1], pch = 19, cex = .2) #add points in the plot
points(x1, x2_neg, col = v_col[1], pch = 19, cex = .2)

# Now let's stack them row wise using rbind() 
x_pos = rbind(x1, x2_pos)
x_pos
x_neg = rbind(x1, x2_neg)
x_neg

# Define a matrix
A = matrix(data = c(7, 4, 2, 8), nrow = 2, ncol = 2)

## transformation of x_pos and x_neg by multiplying them by matrix A
y_pos = A %*% x_pos
y_pos
y_neg = A %*% x_neg
y_neg

#Let's visualize the results
plot(xlim <- c(-44, 44),  ylim <- c(-44, 44), type = "n", xlab = "X1", ylab = "X2",
     main = " set multiplied by a matrix") 
grid()  ### adding grids
points(y_pos[1, ], y_pos[2, ], col = v_col[6], pch = 19, cex = .3)
points(y_neg[1, ], y_neg[2, ], col = v_col[6], pch = 19, cex = .3)

##########################################################################
### ellipse
# Let's generate some points first on the x1
x1 <- seq(-3, 3, length = 1000)

# We'll now generate the points for x2 using 4x1^2 -x2^2 = 6x1-8
# Here for each x1, x2 can take positive and negative values.
x2_pos = sqrt(4*x1^2 - 6*x1+8)  ### x2 = sqrt(4*x1^2 - 6*x1+8)
x2_neg = - sqrt(4*x1^2 - 6*x1+8)  ### x2 = -sqrt(4*x1^2 - 6*x1+8)

#visualize this shape, i.e., Ellipse
par(mfrow =c(1, 2)) 
plot(xlim <- c(-8, 8),  ylim <- c(-8, 8), type = "n", xlab = "X1", ylab = "X2",
     main = "Plot the set") 
grid()  ### adding grids
points(x1, x2_pos, col = v_col[1], pch = 19, cex = .2) #add points in the plot
points(x1, x2_neg, col = v_col[1], pch = 19, cex = .2)

#Now let's stack them row wise using rbind() 
x_pos = rbind(x1, x2_pos)
x_pos
x_neg = rbind(x1, x2_neg)
x_neg

#Define a matrix
A = matrix(data = c(7, 4, 2, 8), nrow = 2, ncol = 2)

## transformation of x_pos and x_neg by multiplying them by matrix A
y_pos = A %*% x_pos
y_pos
y_neg = A %*% x_neg
y_neg

#Let's visualize the results
plot(xlim <- c(-60, 60),  ylim <- c(-80, 70), type = "n", xlab = "X1", ylab = "X2",
     main = " set multiplied by a matrix") 
grid()  ### adding grids
points(y_pos[1, ], y_pos[2, ], col = v_col[6], pch = 19, cex = .3)
points(y_neg[1, ], y_neg[2, ], col = v_col[6], pch = 19, cex = .3)


