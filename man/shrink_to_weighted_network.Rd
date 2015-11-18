\name{shrink_to_weighted_network}
\alias{shrink_to_weighted_network}
\title{ Shrink a repetative edgelist into a weighted }
\description{ This function creates a weighted edgelist from a list of edges where a duplicate means an increase in the weight. }
\usage{shrink_to_weighted_network(net)}
\arguments{
  \item{net}{ can use both undirected and directed edgelist in the following format (sender.id receiver.id):\cr
   1 2\cr
   1 2\cr
   1 2\cr
   1 2\cr
   1 3\cr
   1 3
   }
}
\value{Returns a weighted one-mode network, e.g.,\cr
1 2 4 \cr
1 3 2 }
\references{ http://toreopsahl.com/2008/11/28/network-weighted-network/ }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0}
\examples{
## Load sample data
sample <- rbind(
c(1,2),
c(1,2),
c(1,2),
c(1,2),
c(1,3),
c(1,3),
c(2,1),
c(2,1),
c(2,1),
c(2,1),
c(2,3),
c(2,3),
c(2,3),
c(2,3),
c(2,4),
c(2,5),
c(2,5),
c(3,1),
c(3,1),
c(3,2),
c(3,2),
c(3,2),
c(3,2),
c(4,2),
c(5,2),
c(5,2),
c(5,6),
c(6,5))

## Run the programme
shrink_to_weighted_network(sample)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }