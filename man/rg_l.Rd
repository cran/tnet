\name{rg_l}
\alias{rg_l}
\title{ Random longitudinal network generator }
\description{
  This function creates a random network from a longitudinal dataset.  NOTE NOTE THIS FILE IS STILL NOT DONE!!!
}
\usage{rg_l(net, keep.i = FALSE, keep.j = FALSE, seed = NULL)}
\arguments{
  \item{net}{Data in the longitudinal format}
  \item{keep.i}{ Whether or not the tie creators should be maintained }
  \item{keep.j}{ Whether or not the tie receivers should be maintained }
  \item{seed}{ the random seed. If you want it to be non-reproducable, use NULL otherwise, use a number }
}
\value{  Returns a random longitudinal network.}
\references{ tore@opsahl.co.uk }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0}
\examples{
t <- c('2007-09-12 13:45:00', 
'2007-09-12 13:46:31',
'2007-09-12 13:47:54',
'2007-09-12 13:48:21',
'2007-09-12 13:49:27',
'2007-09-12 13:58:14',
'2007-09-12 13:52:17',
'2007-09-12 13:56:59');
i <- c(1,1,1,1,1,1,1,1);
j <- c(2,2,2,2,2,2,3,3);
w <- c(1,1,1,1,1,1,1,1);
sample <- data.frame(t, i, j, w);

rg_l(sample)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }
