\name{degree_tm}
\alias{degree_tm}
\title{ Degree centrality in a two-mode network }
\description{ This function calculates two degree measures: the number of contacts that a node is connected to, and the sum of weights on ties originating from a node (strength). }
\usage{degree_tm(net,measure=c("degree","output"))}
\arguments{
  \item{net}{A two-mode network }
  \item{measure}{ specifies which measures should be calculated }
}
\value{ Returns a data.frame with two or three columns: the first column contains the nodes' ids, and the remaining columns contain the scores of the measures specified in the measure-parameter. }
\references{ http://toreopsahl.com/blog/ }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0 }
\examples{
## Load sample data
network <- cbind(
i=c(1,1,2,2,2,3,3,4,5,5,6),
p=c(1,2,1,3,4,2,3,4,3,5,5),
w=c(3,5,6,1,2,6,2,1,3,1,2))

## Run the programme
degree_tm(network)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }