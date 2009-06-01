\name{distance_w}
\alias{distance_w}
\title{ Distance in a weighted network }
\description{ The shortest path length, or geodesic distance, between two nodes in a binary network is the minimum number of steps you need to make to go from one of them to the other. This distance is the quickest connection between nodes when all ties are the same. However, in a weighted network, all ties are not the same. }
\usage{ distance_w(edgelist, directed=NULL, gconly=TRUE) }

\arguments{
  \item{edgelist}{A weighted edgelist}
  \item{directed}{ logical, whether the network is directed or undirected. Default is NULL, this means that the function checks whether the edgelist is directed or not.}
  \item{gconly}{ logical, whether the function should only be calculated for the giant component. Default is TRUE.}
}
\value{ Returns a distance matrix. }
\references{ t.opsahl@qmul.ac.uk }
\author{ Tore Opsahl }
\note{ version 1.0.0}
\examples{
## Load sample data
sample <- rbind(
c(1,2,8),
c(1,4,1),
c(2,1,8),
c(2,3,6),
c(3,2,6),
c(3,4,10),
c(4,1,1),
c(4,3,10))

## Run the programme
distance_w(sample)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }