\name{closeness_w}
\alias{closeness_w}
\title{ Closeness in a weighted network }
\description{ This function calculates closeness scores for a weighted network based on the distance\_w. }
\usage{ closeness_w(edgelist, directed=NULL, precomp.dist=NULL) }

\arguments{
  \item{edgelist}{ A weighted edgelist }
  \item{directed}{ Logical: whether the edgelist is directed or undirected. Default is NULL, then the function detects this parameter. }
  \item{precomp.dist}{ If you have already computed the distance matrix using distance\_w, you can enter the name of the matrix object here. }
}
\value{ Returns a data.frame with two columns: the first containts the node ids of all the nodes in the edgelist, and the second containts the corresponding closeness scores. }
\references{ http://toreopsahl.com/2009/01/09/average-shortest-distance-in-weighted-networks/ }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0 }
\examples{
## Load sample data
sampledata <- rbind(
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
closeness_w(sampledata)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }