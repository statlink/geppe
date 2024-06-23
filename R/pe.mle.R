pe.mle <- function(x) {

  pe <- function(vec, x, n, sx) {
    theta <- exp( vec[1] )
    lambda <- exp( vec[2] )
    - n * log( theta * lambda ) + lambda * sx + theta * sum( exp( - lambda * x ) ) + n * log( 1 - exp( - theta ) )
  }

  n <- length(x)  ;  sx <- sum(x)
  f <- optim( par = c(1, 1), fn = pe, x = x, n = n, sx = sx, control = list(maxit = 5000) )
  f <- optim( par = f$par, fn = pe, x = x, n = n, sx = sx, control = list(maxit = 5000) )
  param <- exp( f$par )
  names(param) <- c("theta", "lambda")
  list(param = param, loglik = -f$value)
}
