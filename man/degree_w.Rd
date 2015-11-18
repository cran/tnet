\name{degree_w}
\alias{degree_w}
\title{ Degree centrality in a weighted network }
\description{ This function calculates two degree measures: the number of contacts that a node is connected to, and the sum of weights on ties originating from a node (out-strength). To calculate the reverse (in-degree, in-strength), specify type="in".
}
\usage{degree_w(net,measure=c("degree","output"), type="out", alpha=1)}
\arguments{
  \item{net}{A weighted edgelist}
  \item{measure}{ specifies which measures should be calculated }
  \item{type}{ shall out- or in-measures be calculated? Default is out. For undirected networks, this setting is irrelevant, but must be specified. }
  \item{alpha}{ sets the alpha parameter in the generalised measures from Opsahl, T., Agneessens, F., Skvoretz, J., 2010. Node Centrality in Weighted Networks: Generalizing Degree and Shortest Paths. Social Networks. If this parameter is set to 1 (default), the sum of tie weights is used. This measure simply use the tie weights and disregards the number of nodes on the paths.}
}
\value{ Returns a data.frame with two or more columns: the first column contains the nodes' ids, and the remaining columns contain the scores of the measures specified in the measure-parameter. }
\references{ http://toreopsahl.com/2008/11/28/network-weighted-network/ }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0 }
\examples{
## Load sample data
network <- rbind(
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
degree_w(network)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }