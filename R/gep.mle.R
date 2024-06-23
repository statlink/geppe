gep.mle <- function(x) {

  gep <- function(vec, x, n, sx) {
    beta <- exp( vec[1] )
    alpha <- exp( vec[2] )
    lambda <- exp( vec[3] )
    n * lambda - n * log( alpha * lambda * beta ) + n * alpha * log( 1 - exp( - lambda ) ) -
    ( alpha - 1 ) * sum( log( 1 - exp( - lambda + lambda * exp( - beta * x ) ) ) ) +
    beta * sx - lambda * sum( exp( - beta * x ) )
  }

  n <- length(x)  ;  sx <- sum(x)
  f <- optim( par = c(1, 1, 1), fn = gep, x = x, n = n, sx = sx, control = list(maxit = 5000) )
  f <- optim( par = f$par, fn = gep, x = x, n = n, sx = sx, control = list(maxit = 5000) )
  param <- exp( f$par )
  names(param) <- c("beta", "alpha", "lambda")
  list(param = param, loglik = -f$value)
}
