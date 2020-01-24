\name{tnet_ucinet}
\alias{tnet_ucinet}
\title{ Exports a tnet network to a DL file for UCINET }
\description{
  The tnet_sna function creates a DL file which can easily be imported into UCINET.
}
\usage{tnet_ucinet(net, type=NULL, file=NULL)}
\arguments{
  \item{net}{A tnet network }
  \item{type}{ type of tnet network, see as.tnet. }
  \item{file}{ filename of output file. If this is set to NULL, a file is created in the working directory with the current time (e.g., tnet_ucinet_network-2011-04-10_150817.dl). }
}
\value{ Writes a UCINET dl file.}
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

## Run the program: leaves file in working directory
\donttest{tnet_ucinet(sample, type="weighted one-mode tnet")}

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }
