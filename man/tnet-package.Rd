\name{tnet-package}
\alias{tnet-package}
\alias{tnet}
\docType{package}
\title{
Collection of programmes to deal with weighted and exact time stamped networks
}
\description{
This package is all about analysis of weighted and logitudinal network datasets. Binary relations limit the richness of network relations as all relations are unique. Longitudinal dataset can allow for an understanding of the causal relationship as all elements of a cross-sectional dataset depends on each other.
}
\details{
\tabular{ll}{
Package: \tab tnet\cr
Type: \tab Package\cr
Version: \tab 0.0.6\cr
Date: \tab 2009-06-01 }

This package allows for analysis of weighted network dataset and network dataset where the sequence of arcs is known. More information is available on http://opsahl.co.uk/tnet/

It reads two forms of data structures (the nodes must be listed in a sequence starting from 1):

1) simple weighted data in the following format (vertex.id.of.creator vertex.id.of.target weight.of.tie): \cr
1 2 4\cr
1 3 2\cr

2) timed data in the following format (MySQL-timestamp.surrounded.by.quotes vertex.id.of.creator vertex.id.of.target weight):\cr
"2007-09-12 13:45:00" 1 2 1\cr
"2007-09-12 13:46:31" 1 2 1\cr
If ties are repeated, the tie increases the weighted. The weight column decides how much weight is added at each time (this can take a negative value to decrease the weight). If the weight column takes the value NaN, the tie is removed from the network.

Attribute files are read as follows:\cr
0 1 3 \cr
0 3 2 \cr
1 3 3 \cr
where the first row refers to vertex id 1, the second row to vertex id 2, and so on. The first column refers to the first attributes, second column to the second attribute and so on.
}
\author{
Tore Opsahl 

Maintainer: Tore Opsahl <tore@opsahl.co.uk>
}
\references{
 http://opsahl.co.uk/tnet
}
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
