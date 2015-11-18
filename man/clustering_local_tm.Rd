\name{clustering_local_tm}
\alias{clustering_local_tm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Redefined local clusering coefficient for two-mode networks }
\description{ This function calculates the local two-mode clusering coefficient as proposed in Opsahl, T., 2010. Triadic closure in two-mode networks: Redefining the global and local clustering coefficients. arXiv:1006.0887. }
\usage{clustering_local_tm(net)}
\arguments{
  \item{net}{A binary or weighted two-mode edgelist}
}
\value{ Returns the local clustering coefficient for the primary node set (the first of an edgelist or the rows of a matrix) }
\references{ Opsahl, T., 2010. Triadic closure in two-mode networks: Redefining the global and local clustering coefficients. arXiv:1006.0887   }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0 }

\examples{
# Weighted two-mode network
net <- cbind(
i=c(1,1,2,2,2,3,3,4,5,5,6),
p=c(1,2,1,3,4,2,3,4,3,5,5),
w=c(3,5,6,1,2,6,2,1,3,1,2))

## Run binary clustering function
clustering_local_tm(net[,1:2])

## Run weighted clustering function
clustering_local_tm(net)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }