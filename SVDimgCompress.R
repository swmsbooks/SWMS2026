#install package 'imager'
#install.packages("imager")

#load the package
library(imager)


### Question 2
# # convert image to grayscale
graydog <- load.image("graydog.jpg")
plot(graydog)
dim(graydog)

# which part does this correspond to?
graydog[200,10,1,1]

# store only the matrix of numbers
mat <- graydog[,,1,1]

## SVD and recombination
decomp <- svd(mat)
recomb <- decomp$u %*%diag(decomp$d) %*% t(decomp$v)

# plotting the recombined image
plot(as.cimg(recomb))

## Choosing only 2 top eigenvalues
ind <- 2
lowermat <- decomp$u[,1:ind] %*%diag(decomp$d[1:ind]) %*% t(decomp$v[,1:ind])
#computing matrix with just 2 dimensions.

lower <- as.cimg(lowermat)
#creating image with this lower dimensional matrix.

plot(lower)
#plot image

# will save in your working directory
save.image(as.cimg(lower), file = "lower_dog.jpg")
#save image



## Question 4: Choosing only k top eigenvalues
ind <-   # choose a number
lowermat <- decomp$u[,1:ind] %*%diag(decomp$d[1:ind]) %*% t(decomp$v[,1:ind])
#computing matrix with just k dimensions.

# the distance between original image matrix and compressed one
max(abs(mat - lowermat))

lower <- as.cimg(lowermat)
#creating image with this lower dimensional matrix.

plot(lower)
#plot image

# will save in your working directory
save.image(as.cimg(lower), file = "lower_dog.jpg")
#save image


