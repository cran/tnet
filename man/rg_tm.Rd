\name{rg_tm}
\alias{rg_tm}
\title{ Random binary and weighted two-mode network }
\description{
  Creates random binary and weighted two-mode networks
}
\usage{rg_tm(ni=100,np=100,ties=300,max.weight=1,seed=NULL)}
\arguments{
  \item{ni}{ Number of nodes in the first set }
  \item{np}{ Number of nodes in the second set }
  \item{ties}{ Number of ties }
  \item{max.weight}{ Max weight (see rg_w), if set to 1 a binary network is produced. }
  \item{seed}{ the random seed. If you want it to be non-reproducable, use NULL otherwise, use a number }
}
\value{Returns a random two-mode network }
\references{  }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0 }
\examples{
## Run the programme
rg_tm(ni=10,np=10,ties=20,max.weight=1,seed=NULL)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }

