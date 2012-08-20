\name{as.static.tnet}
\alias{as.static.tnet}
\title{ Transform a longitudinal network to a static edgelist network }
\description{
  This function transforms a longitudinal network to a static edgelist
}
\usage{as.static.tnet(ld)}
\arguments{
  \item{ld}{ Longitudinal network }
}
\value{Returns the data in an edgelist format.}
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
w <- c(1,1,1,1,1,-1,1,1);
net <- data.frame(t, i, j, w);

## Run the programme
as.static.tnet(net)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }