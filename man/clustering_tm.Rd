\name{clustering_tm}
\alias{clustering_tm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Redefined clusering coefficient for two-mode networks }
\description{ This function calculates the two-mode clusering coefficient as proposed by Opsahl, T., 2009. Clustering in binary and weighted two-mode networks. Two-mode conference in Amsterdam. }
\usage{clustering_tm(net)}
\arguments{
  \item{net}{A binary or weighted two-mode edgelist}
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