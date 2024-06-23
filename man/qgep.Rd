\name{Quantile function of the GEP, EP and PE distributions}
\alias{qepois}
\alias{qgep}
\alias{qpe}
\title{
Quantile function of the GEP, EP and PE distributions
}

\description{
Quantile function of the GEP, EP and PE distributions.
}

\usage{
qepois(p, beta, lambda)
qgep(p, beta, alpha, lambda)
qpe(p, theta, lambda)
}

\arguments{
\item{p}{
A numerical vector with probability values.
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
The quantiles of the GEP, EP and PE distributions are computed.

The quantile function of the EP is given by
\eqn{
x_q=-\dfrac{\log\left[\lambda^{-1}\log\left(q\left(1-e^{\lambda}\right)+e^{\lambda}\right)\right]}{\beta}.
}

The quantile function of the GEP is given by
\eqn{
x_q=-\dfrac{\log{\left[1+\lambda^{-1}\log{\left(1-q^{1/\alpha}\left(1-e^{-\lambda}\right)\right)}\right]}}{\beta}.
}

The quantile function of the PE is given by
\eqn{
x_q=\dfrac{\log{\left(\theta\right)}-\log{\left[-\log{\left(q-e^{\theta}\left(q-1\right)\right)}\right]}}{\lambda}.
}
}

\value{
A vector with the quantile values.
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
y <- qgep(seq(0.1, 0.9, by = 0.1), 1, 2, 3)
}

