pepois <- function(x, beta, lambda) {
  ( exp( lambda * exp( - beta * x ) ) - exp( lambda ) ) / ( 1 - exp( lambda ) )
}
