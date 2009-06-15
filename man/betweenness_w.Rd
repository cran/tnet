\name{betweenness_w}
\alias{betweenness_w}
\title{ Betweenness in a weighted network }
\description{ This function calculates betweenness scores for a weighted network based on the distance\_w. \cr
NB: This algorithm relies on the RBGL package's boost-library. The boost C++ implementation of Brandes' (2001) algorithm will be included in tnet from version 0.0.6. This algorithm finds multiple paths if they have exactly the same distance. For example, if one path is found over the direct tie with a weight of 1 (distance = 1/1 = 1) and a second path is through an intermediary node with two ties with weights of 2 (distance = 1/2 + 1/2 = 1), the two paths have exactly the same distance. However, if there is a third path through two intermediaries with three ties with weights of 3 (distance = 1/3 + 1/3 + 1/3), it does not exactly equal 1 as computers read these values as 0.3333333 and the sum of these values is 0.9999999. Therefore, this path is considered shorter than the other two paths (distance = 1).}
\usage{ betweenness_w(edgelist, directed=NULL) }

\arguments{
  \item{edgelist}{ A weighted edgelist }
  \item{directed}{ logical, whether the network is directed or undirected. Default is NULL, this means that the function checks whether the edgelist is directed or not.}
}
\value{ Returns a data.frame with two columns: the first containts the node ids of all the nodes in the edgelist, and the second containts the corresponding betweenness scores. }
\references{ http://toreopsahl.com/2009/02/20/betweenness-in-weighted-networks/ }
\author{ Tore Opsahl; http://toreopsahl.com }
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