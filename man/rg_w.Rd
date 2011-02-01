\name{rg_w}
\alias{rg_w}
\title{ Random weighted network generator }
\description{
  This function creates a random network with random edge weights.
}
\usage{rg_w(nodes=100,arcs=300,weights=1,directed=TRUE,seed=NULL)}
\arguments{
  \item{nodes}{ number of nodes }
  \item{arcs}{ number of arcs; if this value is between 0 and 1, a random network where each tie is based on this probability will be produced }
  \item{weights}{ A tie weight vector to be randomly sampled. }
  \item{directed}{ whether you want a directed or undirected network, values TRUE or FALSE }
  \item{seed}{ the random seed. If you want it to be non-reproducable, use NULL otherwise, use a number }
}
\value{  Returns a random edgelist with random weights.}
\references{ http://opsahl.co.uk/tnet/ }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0}
\examples{
rg_w(nodes=10,arcs=30,directed=FALSE,seed=1)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }