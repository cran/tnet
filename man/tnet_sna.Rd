\name{tnet_sna}
\alias{tnet_sna}
\title{ Exports a tnet network to an sna object }
\description{
  The tnet_sna function creates an sna object from a tnet network.
}
\usage{tnet_sna(net,type=NULL)}
\arguments{
  \item{net}{A tnet network }
  \item{type}{ type of tnet network, see as.tnet. }
}
\value{Returns an sna object.}
\references{ http://toreopsahl.com/ }
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
tnet_sna(sample, type="weighted one-mode tnet")

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }