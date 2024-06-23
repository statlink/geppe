\name{Random values generation from the GEP, EP and PE distributions}
\alias{repois}
\alias{rgep}
\alias{rpe}
\title{
Random values generation from the GEP, EP and PE distributions
}

\description{
Random values generation from the GEP, EP and PE distributions.
}

\usage{
repois(n, beta, lambda)
rgep(n, beta, alpha, lambda)
rpe(n, theta, lambda)
}

\arguments{
\item{n}{
The sample size.
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
In order to generate values from these distributions the inverse rejection sampling is used.
}

\value{
A vector with generated values from the GEP, EP or the PE distribution.
}

\references{
Barreto-Souza W. and Cribari-Neto F. (2009). A generalization of the exponential-Poisson distribution.
Statistics and Probability Letters, 79(24): 2493--2500.

Louzada F., Ramos P. L. and Ferreira H. P. (2020). Exponential-Poisson distribution:
estimation and applications to rainfall and aircraft data with zero occurrence.
Communications in Statistics-Simulation and Computation, 49(4): 1024--1043.

Rodrigues G. C., Louzada F. and Ramos P. L. (2018). Poisson-exponential distribution: different methods of estimation.
Journal of Applied Statistics, 45(1): 128--144.
}

\author{
Sofia Piperaki.

R implementation and documentation: Sofia Piperaki \email{sofiapip23@gmail.com} and Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}
\seealso{
\code{\link{dgep}}
}

\examples{
x <- rgep(100, 1, 2, 3)
}

