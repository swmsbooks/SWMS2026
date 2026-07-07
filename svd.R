####### SWMS 2026 Day4 Worksheet 2 ############
######### Q3 #####################################

##### singular value decomposition (svd) of a matrix
##### using inbuilt function svd() in R

#### write a matrix
A <- matrix(c(2,-1, 0, 0, 2, 1, 2, 1, 4 ), nrow = 3, ncol = 3)
A

#### singular value decomposition (svd) of A
s <- svd(A)
s

### it provides d, u and v
###  create a diagonal matrix of d
D <- diag(s$d)

### see that if we multiply these three matrices U, D and V'
### we get A again
s$u %*% D %*% t(s$v) #  A = U D V'

### similarly you must try it for other matrices given in the exercise

