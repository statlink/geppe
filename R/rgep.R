rgep <- function(n, beta, alpha, lambda) {
  u <- Rfast2::Runif( n )
  ( - beta ** - 1 ) * log( 1 + (lambda ** - 1) * log(1 - u ** ( 1 / alpha ) * ( 1 - exp( - lambda ) ) ) )
}
