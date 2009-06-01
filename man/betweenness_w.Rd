\name{betweenness_w}
\alias{betweenness_w}
\title{ Betweenness in a weighted network }
\description{ This function calculates betweenness scores for a weighted network based on the distance\_w. NB: This algorithm relies on the RBGL package's boost-library. Currently the sp.between funtion in the boost library only finds one shortest path, even if multiple exists. Therefore, this function should be used with caution and not for publishing.}
\usage{ betweenness_w(edgelist, directed=NULL) }

\arguments{
  \item{edgelist}{A weighted edgelist}
  \item{directed}{ logical, whether the network is directed or undirected. Default is NULL, this means that the function checks whether the edgelist is directed or not.}
}
\value{ Returns betweenness score for each vertex in the edgelist. }
\references{ t.opsahl@qmul.ac.uk }
\author{ Tore Opsahl }
\note{ version 1.0.0 }
\examples{
## Load sample data
sampledata <- rbind(
c(1,2,1),
c(1,3,5),
c(2,1,1),
c(2,4,6),
c(3,1,5),
c(3,4,10),
c(4,2,6),
c(4,3,10))

## Run the programme
betweenness_w(sampledata)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }