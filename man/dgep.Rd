\name{Density computation of the GEP, EP and PE distributions}
\alias{depois}
\alias{dgep}
\alias{dpe}
\title{
Density computation of the GEP, EP and PE distributions
}

\description{
Density computation of the GEP, EP and PE distributions.
}

\usage{
depois(x, beta, lambda, logged = FALSE)
dgep(x, beta, alpha, lambda, logged = FALSE)
dpe(x, theta, lambda, logged = FALSE)
}

\arguments{
\item{x}{
A numerical vector with non-negative values.
}
\item{beta}{
A strictly positive number, the scale parameter (\eqn{\beta}).
}
\item{alpha}{
A stritly positive number, the \eqn{\alpha} parameter of the GEP distribution. If \eqn{a=1}, then one ends up with the EP distribution.
}
\item{theta}{
A strictly positive number, the shape parameter (\eqn{\theta}).
}
\item{lambda}{
A strictly positive number, the shape parameter (\eqn{\lambda}).
}
\item{logged}{
Should the logarithm of the density values be computed? The default value is FALSE.
}
}

\details{
The density values of the GEP, EP and PE distributions are computed.
The density function of the EP is given by
\eqn{
f(x)=\dfrac{\lambda \beta e^{-\lambda-\beta x + \lambda e^{-\beta x}}}{1-e^{-\lambda}}.
}

The density function of the GEP is given by
\eqn{
f(x)=\dfrac{\alpha \lambda \beta}{\left(1-e^{-\lambda}\right)^{\alpha}}\left(1-e^{-\lambda+\lambda e^{-\beta x}}\right)^{\alpha-1}e^{-\lambda -\beta x + \lambda e^{-\beta x}}.
}

The density function of the PE is given by
\eqn{
f(x)=\dfrac{\theta \lambda e^{-\lambda x-\theta e^{\lambda x}}}{1-e^{-\theta}}.
}
}

\value{
A vector with the (logged) density values.
}

\references{
Barreto-Souza W. and Cribari-Neto F. (2009). A generalization of the exponential-Poisson distribution.
Statistics and Probability Letters, 79(24): 2493--2500.

Louzada F., Ramos P. L. and Ferreira H. P. (2020). Exponential-Poisson distribution:
estimation and applications to rainfall and aircraft data with zero occurrence.
Communications in Statistics-Simulation and Computation, 49(4): 1024--1043.

Rodrigues G. C., Louzada F. and Ramos P. L. (2018). Poisson-exponential distribution: different methods of estimation. Journal of Applied Statistics, 45(1): 128--144.
}

\author{
Sofia Piperaki.

R implementation and documentation: Sofia Piperaki \email{sofiapip23@gmail.com} and Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}
\seealso{
\code{\link{rgep}, \link{pgep} }
}

\examples{
x <- rgep(100, 1, 2, 3)
y <- dgep(x, 1, 2, 3, logged = TRUE)
sum(y)
}

