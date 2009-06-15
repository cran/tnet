\name{rg_reshuffling_w}
\alias{rg_reshuffling_w}
\title{ Reshuffle of a weighted network }
\description{ This function randomly resuffles a weighted edgelist. }
\usage{ rg_reshuffling_w(edgelist, option="weights", directed=NULL, seed=NULL) }

\arguments{
  \item{edgelist}{A weighted edgelist}
  \item{option}{ what should be reshuffled: 1) weights (default): randomly assigns the weights to the edges; 2) degree: maintain the degree distribution, but changes the contacts randomly. }
  \item{directed}{ logical: is the network directed or undirected. Default: NULL }
  \item{seed}{ seed for random generator, set if you want random yet reproducable results. }
}
\value{ Returns a distance matrix. }
\references{ Molloy, M., Reed, B., 1995. A critical point for random graphs with a given degree sequence. Random Structures and Algorithms 6, 161-180. \cr
Opsahl, T., Colizza, V., Panzarasa, P., Ramasco, J. J., 2008. Prominence and control: The weighted rich-club effect. Physical Review Letters 101 (168702). arXiv:0804.0417. \cr
http://toreopsahl.com/2008/12/12/article-prominence-and-control-the-weighted-rich-club-effect/ }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0}
\examples{
## Load sample data
sampledata<-rbind(
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
c(6,5,1));

## Run the function
rg_reshuffling_w(sampledata, option="weights", directed=FALSE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }