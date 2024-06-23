repois <- function(n, beta, lambda) {
  u <- Rfast2::Runif( n )
  - ( log( ( lambda ** ( - 1 ) ) * log( u * ( 1 - exp( lambda ) ) + exp( lambda ) ) ) ) / beta
}
