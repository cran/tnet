\name{dichotomise_w}
\alias{dichotomise_w}
\title{ Dichotomise a weighted one-mode network into a binary one-mode network }
\description{
  The dichotomise function creates a binary one-mode network from a weighted edgelist.
}
\usage{dichotomise_w(net,GT=0)}
\arguments{
  \item{net}{A weighted one-mode network }
  \item{GT}{ the cut-off parameter. Default is set to 0, so edges/arcs with a weight greater than 0 is set to 1. }
}
\value{Returns the edgelist with edges below the cut-off removed, and all weights equal to 1.}
\references{ http://toreopsahl.com/2008/11/28/network-weighted-network/ }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0 }
\examples{
## Load sample data
sample <- rbind(
c(1,2,4),
c(1,3,2),
c(2,1,4),
c(2,3,4),
c(2,4,1),
c(2,5,2),
c(3,1,2),
c(3,2,4),
c(4,2,1),
c(5,2,2),
c(5,6,1),
c(6,5,1))

## Run the programme
dichotomise_w(sample, GT=2)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }