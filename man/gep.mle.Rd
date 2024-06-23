\name{Maximum likelihood estimation of the GEP, EP and PE distributions}
\alias{epois.mle}
\alias{gep.mle}
\alias{pe.mle}
\title{
Maximum likelihood estimation of the GEP, EP and PE distributions
}

\description{
Maximum likelihood estimation of the GEP, EP and PE distributions.
}

\usage{
epois.mle(x)
gep.mle(x)
pe.mle(x)
}

\arguments{
\item{x}{
A numerical vector with non negative values.
}
}

\details{
Maximum likelihood estimation of the EP, GEP and PE distributions is performed.
}

\value{
A list including:
\item{param}{
A vector with the estimated values of \eqn{\alpha}, \eqn{\beta}, \eqn{\theta}, \eqn{\lambda}, depending on the distribution used.
}
\item{loglik}{
The log-likelihood value of the distribution.
}
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
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}
\seealso{
\code{\link{rgep}}
}

\examples{
x <- repois( 1000, 1, 3)
epois.mle(x)
}

