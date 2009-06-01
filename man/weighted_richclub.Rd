\name{weighted_richclub}
\alias{weighted_richclub}
\title{ The weighted rich-club effect }
\description{
  This function calculates the weighted rich-club coefficient proposed in Opsahl, T., Colizza, V., Panzarasa, P., Ramasco, J.J., 2008. Prominence and control: The weighted rich-club effect. PRL 101. 
}
\usage{weighted_richclub(edgelist, rich="k", reshuffle="weights", samples=100, 
seed=NULL, directed=NULL)}
\arguments{
  \item{edgelist}{A weighted edgelist}
  \item{rich}{ specifies the richness parameter, either "k" or "s". }
  \item{reshuffle}{ specifies the reshuffling procedure used, either "weights" or "links". }
  \item{samples}{ number of random networks used. }
  \item{seed}{ the random generators seed, used to produce random yet reproducable results. }
  \item{directed}{ logical parameter: whether the network is directed or undirected. }
}
\value{  Returns a table with the fraction of phi(observed) over phi(null) for each k or s in the dataset.}
\references{ Opsahl et al., 2008. Prominence and control: The weighted rich-club effect. PRL 101 }
\author{ Tore Opsahl t.opsahl@qmul.ac.uk }
\note{ version 1.0.0 }
\examples{
## Load sample data
sampledata <- rbind(
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
c(6,5,1))

## Run the function
weighted_richclub(sampledata, rich="k", reshuffle="weights")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }