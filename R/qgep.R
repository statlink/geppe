qgep <- function(p, beta, alpha, lambda) {
  ( - beta ** - 1 ) * log( 1 + ( lambda ** - 1 ) * log( 1 - p ** ( 1 / alpha )*( 1 - exp( - lambda ) ) ) )
}
