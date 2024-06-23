qpe <- function(p, theta, lambda) {
  ( log( theta ) - log( - log( p - exp( - theta ) *( p - 1 ) ) ) ) / lambda
}