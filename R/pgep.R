pgep <- function(x, beta, alpha, lambda) {
  ( ( 1 - exp( - lambda + lambda * exp( - beta * x ) ) ) / ( 1 - exp( - lambda) ) ) ** alpha
}
