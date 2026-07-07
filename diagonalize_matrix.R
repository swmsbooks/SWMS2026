####### SWMS 2026 Day4 Worksheet 2 ############
######### Q1 #####################################

#### diagonalization of a matrix A

#### creating a 3 x 3 matrix using matrix()
A <- matrix(c(3, 5, 8, 2, 7, 3, 2, 2, 1), nrow = 3, ncol=3)
A

#### find eigen values of A using eigen()$values
eigval_A <- eigen(A)$values
eigval_A

#### find the eigen vectors of A using eigen()$values
S <- eigen(A)$vectors 
S

#### eigen() gives eigen values and eigen vectors of a matrix
#### to see this just run eigen(A)

#### find the inverse of S using solve()
S_inv <- solve(S)
S_inv

# Place eigen values as a diagonal matrix using diag()
D <- diag(eigval_A)
D

#### diagonalization of a matrix A
diagonalization <- S %*% D %*% S_inv
diagonalization

