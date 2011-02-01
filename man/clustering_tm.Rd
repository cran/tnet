\name{clustering_tm}
\alias{clustering_tm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Redefined clusering coefficient for two-mode networks }
\description{ This function calculates the two-mode clusering coefficient as proposed by Opsahl, T., 2009. Clustering in binary and weighted two-mode networks. Two-mode conference in Amsterdam.\cr
Note: If you are having problems with this function (i.e., run out of memory), there is a much more memory efficient c++ function. However, this function is not fully integrated in R, and requires a few extra steps. }
\usage{clustering_tm(net, subsample=1, seed=NULL)}
\arguments{
  \item{net}{A binary or weighted two-mode edgelist}
  \item{subsample}{Whether a only a subset of 4-paths should we used when calculating the measure. This is particularly useful when running out of memory analysing large networks. If it is set to 1, all the 4-paths are analysed. If it set to a value below one, this is roughly the proportion of 4-paths that will be analysed. If it is set to an interger greater than 1, this number of ties that form the first part of a 4-path that will be analysed.}
  \item{seed}{If a subset of 4-paths is analysed, by setting this parameter, the results are reproducable.}
}
\value{Returns the outcome of the equation presented in the paper}
\references{ Opsahl, T., 2009. Clustering in binary and weighted two-mode networks. Two-mode conference in Amsterdam. }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0 }

\examples{
# Weighted two-mode network
net <- cbind(
i=c(1,1,2,2,2,3,3,4,5,5,6),
p=c(1,2,1,3,4,2,3,4,3,5,5),
w=c(3,5,6,1,2,6,2,1,3,1,2))

## Run binary clustering function
clustering_tm(net[,1:2])

## Run weighted clustering function
clustering_tm(net)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }