\name{distance_tm}
\alias{distance_tm}
\title{ Distance in a two-mode network }
\description{ The shortest path length, or geodesic distance, between two nodes in a binary network is the minimum number of steps you need to make to go from one of them to the other. See the distance_w-function for more details. }
\usage{ distance_tm(net, projection.method="sum", gconly=TRUE,subsample=1, seed=NULL) }
\arguments{
  \item{net}{A two-mode network }
  \item{projection.method}{The way the two-mode network is projected. The sum method defines tie weights as the number of common nodes (e.g., events, projects etc) that two individuals had contact through. In certain cases, "Newman" might be better. See the projecting_tm-function.}
  \item{gconly}{ logical, whether the function should only be calculated for the giant component. Default is TRUE.}
  \item{subsample}{Whether a only a subset of starting nodes should we used when calculating the measure. This is particularly useful when running out of memory analysing large networks. If it is set to 1, all distances are analysed. If it set to a value below one, this is roughly the proportion of starting noes that will be analysed. If it is set to an interger greater than 1, this number of starting nodes that will be analysed.}
  \item{seed}{If a subset of starting nodes is analysed, by setting this parameter, the results are reproducable.}
}
\value{ Returns a distance matrix. }
\references{ http://toreopsahl.com/2009/01/09/average-shortest-distance-in-weighted-networks/ }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0}
\examples{
# Load networks
net <- cbind(
 i=c(1,1,2,2,2,3,3,4,5,5,6),
 p=c(1,2,1,3,4,2,3,4,3,5,5),
 w=c(3,5,6,1,2,6,2,1,3,1,2))

# Run the function
distance_tm(net)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }