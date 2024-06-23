epois.mle <- function(x) {

  ep <- function(vec, x, n, sx) {
  beta <- exp( vec[1] )
  lambda <- exp( vec[2] )
  - n * log( lambda * beta ) + n * log( 1 - exp( - lambda ) ) + n * lambda + beta * sx - lambda * sum( exp( - beta * x ) )
  }

  n <- length(x)  ;  sx <- sum(x)
  f <- optim( par = c(1, 1), fn = ep, x = x, n = n, sx = sx, control = list(maxit = 5000) )
  f <- optim( par = f$par, fn = ep, x = x, n = n, sx = sx, control = list(maxit = 5000) )
  param <- exp( f$par )
  names(param) <- c("beta", "lambda")
  list(param = param, loglik = -f$value)
}
