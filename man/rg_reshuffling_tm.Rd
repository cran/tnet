\name{rg_reshuffling_tm}
\alias{rg_reshuffling_tm}
\title{ Reshuffle of a binary two-mode network }
\description{ This function randomly resuffles a binary two-mode edgelist. }
\usage{ rg_reshuffling_tm(net, seed=NULL) }

\arguments{
  \item{net}{A two-mode edgelist}
  \item{seed}{ seed for random generator, set if you want random yet reproducable results. }
}
\value{ Returns a binary two-mode edgelist. }
\references{ http://toreopsahl.com/2009/05/29/weighted-rich-club-effect-a-more-appropriate-null-model-for-scientific-collaboration-networks/ }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0}
\examples{
## Load  data
data(Newman.Condmat.95.99)

## Run the function on a subset
rg_reshuffling_tm(Newman.Condmat.95.99.net.2mode[1:100,], seed=1)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }