\name{rg_tm}
\alias{rg_tm}
\title{ Random binary and weighted two-mode network }
\description{
  Creates classical random binary and weighted two-mode networks
}
\usage{rg_tm(ni=100,np=100,ties=300,weights=1,seed=NULL)}
\arguments{
  \item{ni}{ Number of nodes in the first set }
  \item{np}{ Number of nodes in the second set }
  \item{ties}{ Number of ties; if this value is between 0 and 1, a random network where each tie is based on this probability will be produced }
  \item{weights}{ A tie weight vector to be randomly sampled. If set to 1 (default), all tie weights will be 1, and hence a binary two-mode network will be created. }
  \item{seed}{ the random seed. If you want it to be non-reproducable, use NULL otherwise, use a number }
}
\value{Returns a random two-mode network }
\references{ Tore Opsahl. Triadic closure in two-mode networks: Redefining the global and local clustering coefficients. arXiv:1006.0887 }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0 }
\examples{
## Run the programme
rg_tm(ni=10,np=10,ties=20)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }

