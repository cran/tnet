\name{weighted_richclub_tm}
\alias{weighted_richclub_tm}
\title{ The weighted rich-club effect (two-mode networks) }
\description{
  This function calculates the weighted rich-club coefficient proposed in Opsahl, T., Colizza, V., Panzarasa, P., Ramasco, J.J., 2008. Prominence and control: The weighted rich-club effect. PRL 101. It incorporates two extentions:\cr
  1) a new way of reshuffling (two-mode reshuffling; \cr
  http://toreopsahl.com/2009/05/29/weighted-rich-club-effect-a-more-appropriate-null-model-for-scientific-collaboration-networks/)\cr
  2) calculating significance levels if there are more than 100 random networks (see my PhD thesis; http://toreopsahl.com/publications/thesis/)
}
\usage{weighted_richclub_tm(net, NR=1000, seed=NULL, projection.method="Newman", nbins=30)}
                          
\arguments{
  \item{net}{A binary two-mode edgelist}
  \item{NR}{ number of random networks used. }
  \item{seed}{ the random generators seed, used to produce random yet reproducable results. }
  \item{projection.method}{ the method used to project the two-mode network to a weighted one-mode network: either "sum" or "Newman" }
  \item{nbins}{ the number of bins in the output }
}
\value{  Returns a table with the fraction of phi(observed) over phi(null). Nbins controls the number of rows.}
\references{ Opsahl et al., 2008. Prominence and control: The weighted rich-club effect. PRL 101\cr
http://toreopsahl.com/2008/12/12/article-prominence-and-control-the-weighted-rich-club-effect/ \cr
http://toreopsahl.com/2009/05/29/weighted-rich-club-effect-a-more-appropriate-null-model-for-scientific-collaboration-networks/ }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0 }
\examples{
## Load  data
data(Newman.Condmat.95.99)

## Run the function on a subset
weighted_richclub_tm(Newman.Condmat.95.99.net.2mode[1:100,], NR=10)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }