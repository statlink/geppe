\name{Distribution function of the GEP, EP and PE distributions}
\alias{pepois}
\alias{pgep}
\alias{ppe}
\title{
Distribution function of the GEP, EP and PE distributions
}

\description{
Distribution function of the GEP, EP and PE distributions.
}

\usage{
pepois(x, beta, lambda)
pgep(x, beta, alpha, lambda)
ppe(x, theta, lambda)
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
}

\details{
The cumulative distribution values of the GEP, EP and PE distributions are computed.
The probability function of the EP is given by
\eqn{
f(x)=\dfrac{e^{\lambda e^{-\beta x}}}{1-e^{\lambda}}.
}

The probability function of the GEP is given by
\eqn{
f(x)=\left(\dfrac{1-e^{-\lambda+\lambda e^{-\beta x}}}{1-e^{-\lambda}}\right)^{\alpha]}.
}

The probability function of the PE is given by
\eqn{
f(x)=\dfrac{1-e^{\theta-\theta e^{-\lambda x}}}{1-e^{-\theta}}.
}
}

\value{
A vector with the cumulative distribution density values.
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
\code{\link{dgep}, \link{qgep} }
}

\examples{
x <- rgep(100, 1, 2, 3)
y <- pgep(x, 1, 2, 3)
}

