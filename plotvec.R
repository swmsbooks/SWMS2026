# SWMS 2026 day2 worksheet-2 R codes
# Q1 #Visualizing vectors in R
###load the following library to use viridis colours in plots
library(viridisLite)

## generating viridis colour palatte of size 14
v_col <- viridis(n = 14, option = "H")


####################################################################################
# plotting a vector in 2D plane
# Define the vector in question 1(a)
a <- c(3, 4)

## plotting window and grid for a vector
# make  an empty plot with required axes
plot(xlim <- c(-12 ,12), ylim <- c(-9, 8), type = "n", xlab = "X1", ylab = "X2") 
grid() #add grid lines

# Add vector and point for a vector
arrows(x0 = 0, y0 = 0, x1 =a[1], y1 = a[2], col = v_col[1])
points(a[1], a[2], pch = 19, cex = 1, col = v_col[2])
text(2.5, 2.5, "a", col = v_col[2])

