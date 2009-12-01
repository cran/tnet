\name{small_world_test_w}
\alias{small_world_test_w}
\title{ Finds the observed and randomly expected clustering coefficient and average shortest distance }
\description{
  Finds the observed and randomly expected values of the global clustering coefficient and average shortest (geodesic) distance in weighted networks. A network is considered to be a small world if the observed values are comparable to the random ones. The summary information is printed to the screen, and the underlying information is returned as a list. Based on this list, significance can be calculated (e.g., whether the observed values are within the 95% confidence interval of the random values).
}
\usage{small_world_test_w(net, NR=1000, step=c(1,2,3))}
\arguments{
  \item{net}{A weighted edgelist}
  \item{NR}{Number of random networks}
  \item{step}{ Which steps to perform: 1) calculating values on observed network, 2) calculating values on weight reshuffled networks, and 3) calculating values on link reshuffled networks. }
}
\value{ Summary information is written to the screen, and detailed information is returned as follows: \cr
[[1]][[1]]\cr
This is variable 1, which is the weighted clustering coefficient: clustering_w(net, measure=c("am", "gm", "ma", "mi","bi"))\cr
[[2]]\cr
This is variable 2, which is binary distance matrix: distance_w(net.b)\cr
[[3]]\cr
This is variable 3, which is weighted distance matrix: distance_w(net)\cr
[[4]]\cr
This is variable 4, which is matrix with the results from the weight reshuffled random networks (rows) and different measures (columns), which are \cr
  1 to 5: clustering_w(net.r, measure=c("am", "gm", "ma", "mi","bi"))\cr
  6: average binary distance\cr
  7: average weighted distance\cr
  8: normalised weighted distance\cr
  9: size of giant component\cr
[[5]]\cr
This is variable 5, which is matrix with the results from the link reshuffled random networks (rows) and different measures (columns), which are \cr
  1 to 5: clustering_w(net.r, measure=c("am", "gm", "ma", "mi","bi"))\cr
  6: average binary distance\cr
  7: average weighted distance\cr
  8: normalised weighted distance\cr
  9: size of giant component\cr
}
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0 }
\examples{
## Load sample data
sample <- rbind(
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
small_world_test_w(sample, NR=2)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }