qepois <- function(p, beta, lambda) {
  - ( log( ( lambda ** ( - 1 ) ) * log( p * ( 1 - exp( lambda ) ) + exp( lambda ) ) ) ) / beta
}
