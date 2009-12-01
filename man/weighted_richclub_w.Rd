\name{weighted_richclub_w}
\alias{weighted_richclub_w}
\title{ The weighted rich-club effect }
\description{ This function calculates the weighted rich-club coefficient proposed in Opsahl, T., Colizza, V., Panzarasa, P., Ramasco, J.J., 2008. Prominence and control: The weighted rich-club effect. PRL 101. \cr
http://toreopsahl.com/2008/12/12/article-prominence-and-control-the-weighted-rich-club-effect/ \cr Note that when specifying rich="s", you should also specify reshuffle="weights.local" as this method maintains the out-strength of nodes. }
\usage{weighted_richclub_w(net, rich="k", reshuffle="weights", NR=100, nbins=30, seed=NULL, directed=NULL)}
\arguments{
  \item{net}{A weighted edgelist}
  \item{rich}{ specifies the richness parameter, either "k" or "s". }
  \item{reshuffle}{ specifies the reshuffling procedure used, either "weights" or "links". }
  \item{NR}{ number of random networks used. }
  \item{nbins}{ the number of bins in the output }
  \item{seed}{ the random generators seed, used to produce random yet reproducable results. }
  \item{directed}{ logical parameter: whether the network is directed or undirected. }
}
\value{  Returns a table with the fraction of phi(observed) over phi(null) for each k or s in the dataset.}
\references{ Opsahl et al., 2008. Prominence and control: The weighted rich-club effect. PRL 101\cr
http://toreopsahl.com/2008/12/12/article-prominence-and-control-the-weighted-rich-club-effect/ }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0 }
\examples{
## Load sample data
sample <- cbind(
i=c(1,1,2,2,2,2,3,3,4,5,5,6),
j=c(2,3,1,3,4,5,1,2,2,2,6,5),
w=c(4,2,4,4,1,2,2,4,1,2,1,1))

## Run the function
weighted_richclub_w(sample, rich="k", reshuffle="weights")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }