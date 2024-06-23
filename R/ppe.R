ppe <- function(x, theta, lambda) {
  ( exp (- theta * exp( - lambda * x ) ) - exp(- theta) ) / ( 1 - exp( - theta ) )
}

