\name{clustering_tm_local}
\alias{clustering_tm_local}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Redefined local clusering coefficient for two-mode networks }
\description{ This function calculates the local two-mode clusering coefficient as proposed in Opsahl, T., 2010. Triadic closure in two-mode networks: Redefining the global and local clustering coefficients. Available on http://toreopsahl.com/publications/ }
\usage{clustering_tm_local(net)}
\arguments{
  \item{net}{A binary or weighted two-mode edgelist}
}
\value{Returns the local clustering coefficient for the primary node set (the first column) }
\references{ Opsahl, T., 2010. Triadic closure in two-mode networks: Redefining the global and local clustering coefficients. Available on http://toreopsahl.com/publications/  }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0 }

\examples{
# Weighted two-mode network
net <- cbind(
i=c(1,1,2,2,2,3,3,4,5,5,6),
p=c(1,2,1,3,4,2,3,4,3,5,5),
w=c(3,5,6,1,2,6,2,1,3,1,2))

## Run binary clustering function
clustering_tm_local(net[,1:2])

## Run weighted clustering function
clustering_tm_local(net)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }