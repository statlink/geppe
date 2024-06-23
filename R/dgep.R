dgep <- function(x, beta, alpha, lambda, logged = FALSE) {
  lf <- log( alpha * lambda * beta ) - alpha * log( 1 - exp( - lambda ) ) + ( alpha - 1 ) * log( 1 - exp( - lambda + lambda * exp( - beta * x ) ) ) - lambda - beta * x + lambda * exp( - beta * x )
  if ( !logged ) {
    lf <- exp( lf )
  } # end if
  lf
}
