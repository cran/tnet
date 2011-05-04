\name{compress_ids}
\alias{compress_ids}
\title{ Remove non-active nodes from one-mode/two-mode/longitudinal networks }
\description{
  The compress_ids function removes non-active nodes from one-mode/two-mode/longitudinal networks. 
}
\usage{compress_ids(net,type=NULL)}
\arguments{
  \item{net}{A network in an edgelist or matrix format. See as.tnet }
  \item{type}{ See as.tnet }
}
\value{Returns a list with either 2 or 3 objects. The first one is the network with the compressed id. The second object is the translation table between the original node identification numbers and the newly assigned. For two-mode networks, the second object is the translation table of the primary nodes, and the third object is the translation table for the secondary nodes .}
\references{ http://toreopsahl.com/ }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0 }
\examples{
## Load sample data
t <- c("2007-09-12 13:45:00",
  "2007-09-12 13:45:00",
  "2007-09-12 13:45:01",
  "2007-09-12 13:46:31",
  "2007-09-12 13:46:31",
  "2007-09-12 13:47:54",
  "2007-09-12 13:48:21",
  "2007-09-12 13:49:27",
  "2007-09-12 13:49:27",
  "2007-09-12 13:52:17",
  "2007-09-12 13:56:59",
  "2007-09-12 13:58:14")
i <- c(1,2,1,3,1,2,3,5,1,3,1,1);
j <- c(1,2,2,3,3,1,2,5,5,2,3,5);
w <- c(1,1,1,1,1,1,1,1,1,1,1,1);
samplenet <- data.frame(t, i, j, w);

## Run the function
compress_ids(samplenet)


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }