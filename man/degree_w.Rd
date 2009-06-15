\name{degree_w}
\alias{degree_w}
\title{ Degree in a weighted network }
\description{ This function calculates two degree measures: the number of contacts that a node is connected to, and the sum of weights on ties originating from a node (out-strength). To calculate the reverse (in-degree, in-strength), specify type="in".
}
\usage{degree_w(edgelist,measure=c("degree","output"), self.loops=FALSE,type="out")}
\arguments{
  \item{edgelist}{A weighted edgelist}
  \item{measure}{ specifies which measures should be calculated }
  \item{self.loops}{ this is a switch to control whether self-loops, or the diagonal of an adjacency matrix, is included in the measure. }
  \item{type}{ shall out- or in-measures be calculated? Default is out. For undirected networks, this setting is irrelevant, but must be specified. }
}
\value{ Returns a data.frame with two or three columns: the first containts the node ids of all the nodes in the edgelist, and the second and/or third containt the corresponding degree and/or strength scores. }
\references{ http://toreopsahl.com/2008/11/28/network-weighted-network/ }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0 }
\examples{
## Load sample data
sample.network.1 <- rbind(
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
degree_w(sample.network.1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }