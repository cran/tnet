\name{small_world_test_tm}
\alias{small_world_test_tm}
\title{ Finds the observed and randomly expected clustering coefficient and average shortest distance }
\description{
  Finds the observed and randomly expected values of the global clustering coefficient and average shortest (geodesic) distance in two-mode networks. A network is considered to be a small world if the observed values are comparable to the random ones. The summary information is printed to the screen, and the underlying information is returned as a list. Based on this list, significance can be calculated (e.g., whether the observed values are within the 95% confidence interval of the random values).
}
\usage{small_world_test_tm(net, NR=1000, step=c(1,2))}
\arguments{
  \item{net}{A weighted edgelist}
  \item{NR}{Number of random networks}
  \item{step}{ Which steps to perform: 1) calculating values on observed network, and 2) calculating values on link reshuffled networks. }
}
\value{ Summary information is written to the screen, and detailed information is returned as follows: \cr
[[1]]\cr
This is variable 1, which is the clustering coefficient: clustering_tm(net)\cr
[[2]]\cr
This is variable 2, which is binary distance matrix: distance_w(projecting_tm(net, "binary"))\cr
[[3]]\cr
This is variable 3, which is matrix with the results from the weight reshuffled random networks (rows) and different measures (columns), which are \cr
  1: clustering_tm(net.r)\cr
  2: average binary distance\cr
  3: size of giant component\cr
}
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0 }
\examples{
## Load sample data
sample <- cbind(
i=c(1,1,2,2,2,3,3,4,5,5,6),
p=c(1,2,1,3,4,2,3,4,3,5,5))

## Run the programme
small_world_test_tm(sample, NR=2)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }