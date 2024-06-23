dpe <- function(x, theta, lambda, logged = FALSE) {
  lf <- log( theta ) + log( lambda ) - lambda * x - theta * exp( - lambda * x ) - log( 1 - exp( - theta ) )
  if ( !logged ) {
    lf <- exp( lf ) 
  } # end if
  lf
}