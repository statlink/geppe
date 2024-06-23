rpe <- function(n, theta, lambda) {
  u <- Rfast2::Runif( n )
  ( log( theta ) - log( - log( u - exp( - theta ) * ( u - 1 ) ) ) ) / lambda
}