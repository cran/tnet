\name{weighted_richclub_local_w}
\alias{weighted_richclub_local_w}
\title{ The weighted rich-club effect (local measure) }
\description{ This function calculates the local weighted rich-club coefficient proposed in Opsahl, T., 2008. Local weighted rich-club measure. \cr
http://toreopsahl.com/2008/12/26/local-weighted-rich-club-measure/ }
\usage{weighted_richclub_local_w(net, prominence)}
\arguments{
  \item{net}{A weighted edgelist}
  \item{prominence}{ A vector with 1 denoting prominent, and 0 non-prominent. This list must be as long as the highest node id number. }
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
prominence <- c(1,1,1,0,0,0)

## Run the function
weighted_richclub_local_w(sample, prominence)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }