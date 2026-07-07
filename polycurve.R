##Worksheet 2################
##Q3#########################

#### curve sketching for a one dimensional function

######### load the following libraries
library(viridisLite)  ## to use viridis colours in plots

## generating viridis colour palatte of size, i.e., n = 3
v_col <- viridis(n = 3, option = "D") 
## here "D" stands for viridis
## you can change option from "A" to "H" for various color possibilities

### define function f(x)
f <- function(x){
  x^4 - 8 * x^2 + 15
}

#### plot f(x) over a given domain [-3, 3]
#### using base R function curve()
curve(f, from = -3, to = 3, col = v_col[1], lwd = 2,
      xlab = "x",
      ylab = "f(x)",
      main = "Plot of f(x) and its derivatives")
grid()
### obtaining zeros of f(x)
### using polyroot function
### which takes coefficients as input in increasing order of the polynomial
f.zeros <- polyroot(c(15, 0, -8, 0, 1))
f.zeros

### note that polyroot gives zeros in complex number form 
### but actually they are real numbers 
### so we use Re() to extract the real part
f.zeros <- Re(f.zeros)

### adding zeros of f(x) in the plot
### using base R function points()
points(f.zeros, f(f.zeros), col = v_col[1], pch = 16)

#### f'(x) 
f.prime <- function(x){
  4 * x^3 - 16 * x
}

### obtaining zeros of f'(x) using polyroot 
fprime.zeros <- polyroot(c(0, -16, 0, 4))
fprime.zeros

### extracting real parts
fprime.zeros <- Re(fprime.zeros)

### adding zeros of f'(x) in the plot
points(fprime.zeros, f.prime(fprime.zeros), col = v_col[2], pch = 16)

####  f''(x) 
f.dobprime <- function(x){
  12* x^2 - 16
}

### obtaining zeros of f''(x) using polyroot 
fdobprime.zeros <- polyroot(c(-16, 0, 12))
fdobprime.zeros

### extracting real parts
fdobprime.zeros <- Re(fdobprime.zeros)

### adding zeros of f''(x) in the plot
points(fdobprime.zeros, f.dobprime(fdobprime.zeros), col = v_col[3], pch = 16)

### adding labels (similar to gf_labs)
title(xlab = "x",
      ylab = "f(x)",
      main = "Plot of f(x) and its derivatives")