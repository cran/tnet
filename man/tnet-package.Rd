\name{tnet-package}
\alias{tnet-package}
\alias{tnet}
\docType{package}
\title{ Collection of function for analysing weighted networks, two-mode networks, and longitudinal networks }
\description{
This package is all about analysis of weighted networks, two-mode networks, and longitudinal networks. Binary relations limit the richness of network relations as all relations are unique. The two-mode structure contains a number of features lost when projection. Longitudinal dataset can allow for an understanding of the causal relationship as all elements of a cross-sectional dataset depends on each other.
}
\details{
\tabular{ll}{
Package: \tab tnet\cr
Type: \tab Package\cr
Version: \tab 0.1.0\cr
Date: \tab 2009-06-15 }

This package allows for analysis of weighted network, two-mode network, and longitudinal network datasets. More information is available on http://opsahl.co.uk/tnet/ and http://toreopsahl.com

It reads three forms of data structures:

1) simple weighted data in the following format (vertex.id.of.creator vertex.id.of.target weight.of.tie): \cr
1 2 4\cr
1 3 2\cr

2) two-mode data in the following format (vertex.id.of.node.in.set.1 vertex.id.of.node.in.set.2 optional.weight.of.tie): \cr
1 1 1\cr
2 1 2\cr

3) timed data in the following format (MySQL-timestamp.surrounded.by.quotes vertex.id.of.creator vertex.id.of.target weight):\cr
"2007-09-12 13:45:00" 1 2 1\cr
"2007-09-12 13:46:31" 1 2 1\cr
If ties are repeated, the tie increases the weighted. The weight column decides how much weight is added at each time (this can take a negative value to decrease the weight). 

Attribute files are read as follows:\cr
0 1 3 \cr
0 3 2 \cr
1 3 3 \cr
where the first row refers to vertex id 1, the second row to vertex id 2, and so on. The first column refers to the first attributes, second column to the second attribute and so on.
}
\references{ http://opsahl.co.uk/tnet/ }
\author{ Tore Opsahl; http://toreopsahl.com }
%\keyword{ networks }
\seealso{
SNA \code{\link[sna:sna-package]{sna}} 
}
\examples{
# Generate a random weighted graph
rg <- rg_w(nodes=100,arcs=300,directed=TRUE)

# Calculate clustering coefficient
clustering_w(rg)
}
