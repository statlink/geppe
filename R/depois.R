depois <- function(x, beta, lambda, logged = FALSE) {
  lf <- log( lambda ) + log( beta ) - lambda - beta * x + lambda  * exp( - beta * x ) - log( 1 - exp( - lambda ) )
  if ( !logged ) {
    lf <- exp( lf )
  } # end if
  lf
}
