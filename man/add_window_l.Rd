\name{add_window_l}
\alias{add_window_l}
\title{ Add smoothing window to a longitudinal dataset }
\description{
  This function adds a smoothing window to a longitudinal dataset
}
\usage{add_window_l(net,window=21, remove.nodes=FALSE)}
\arguments{
  \item{net}{Data in the longitudinal format}
  \item{window}{Number of days before ties 'expire'.}
  \item{remove.nodes}{Whether or not nodes should be removed from the network if they have no ties for the length of the window (i.e., a self-loop with a negative weight)}.
}
\value{Returns the data with negative arcs.}
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

## Run the programme
add_window_l(sample, window=21)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }