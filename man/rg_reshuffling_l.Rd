\name{rg_reshuffling_l}
\alias{rg_reshuffling_l}
\title{ Reshuffling a longitudinal network }
\description{
  This function reshuffles a longitudinal dataset.
}
\usage{rg_reshuffling_l(net, keep.i = FALSE, keep.j = FALSE, seed = NULL)}
\arguments{
  \item{net}{ Longitudinal network }
  \item{keep.i}{ Whether or not the tie creators should be maintained }
  \item{keep.j}{ Whether or not the tie receivers should be maintained }
  \item{seed}{ the random seed. If you want it to have reproducable result, set using an integer }
}
\value{  Returns a reshuffled longitudinal network }
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

rg_reshuffling_l(sample)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }
