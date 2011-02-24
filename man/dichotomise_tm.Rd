\name{dichotomise_tm}
\alias{dichotomise_tm}
\title{ Dichotomise a weighted two-mode network into a binary two-mode network }
\description{
  The dichotomise function creates a binary two-mode network from a weighted edgelist.
}
\usage{dichotomise_tm(net,GT=0)}
\arguments{
  \item{net}{A weighted two-mode network }
  \item{GT}{ the cut-off parameter. Default is set to 0, so edges/arcs with a weight greater than 0 is set to 1. }
}
\value{Returns the edgelist with edges below the cut-off removed, and all weights equal to 1.}
\references{ http://toreopsahl.com/2008/11/28/network-weighted-network/ }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0 }
\examples{
## Load sample data
sample <- cbind(
 i=c(1,1,2,2,2,3,3,4,5,5,6),
 p=c(1,2,1,3,4,2,3,4,3,5,5),
 w=c(3,5,6,1,2,6,2,1,3,1,2))

## Run the programme
dichotomise_tm(sample, GT=2)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }