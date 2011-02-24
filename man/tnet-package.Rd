\name{tnet-package}
\alias{tnet-package}
\alias{tnet}
\docType{package}
\title{ Collection of functions for analysing weighted networks, two-mode networks, and longitudinal networks }
\description{ This package is created to analyse weighted networks, two-mode networks, and longitudinal networks datasets. Binary ties limit the richness of network analyses as relations are unique. The two-mode structure contains a number of features lost when projection it to a one-mode network. Longitudinal datasets allow for an understanding of the causal relationship among ties, which is not the case in cross-sectional datasets as ties are dependent upon each other. }
\details{
\tabular{ll}{
Package: \tab tnet\cr
Type: \tab Package\cr
Version: \tab 3.0.2\cr
Date: \tab 2011-02-24 }

This package is created to analyse weighted networks, two-mode networks, and longitudinal networks datasets. More information is available on http://opsahl.co.uk/tnet/ and http://toreopsahl.com

It utilises three forms of data structures (it can automatically convert matrices etc into these formats, see the as.tnet-function):

1) simple weighted data in the following format (creator.node.id target.node.id tie.weight): \cr
1 2 4\cr
1 3 2\cr
Note: For undirected networks, each tie must be mentioned twice (see the symmetrise_w-function). For example, \cr
1 2 4\cr
2 1 4\cr
1 3 2\cr
3 1 2\cr

2) two-mode data in the following format (primary.node.id secondar.node.id tie.weight.optional): \cr
1 1 1\cr
2 1 2\cr

3) timed data in the following format (MySQL-timestamp.as.character.string creator.node.id target.node.id tie.weight):\cr
"2007-09-12 13:45:00" 1 2 1\cr
"2007-09-12 13:46:31" 1 2 1\cr
If ties are repeated, the tie increases the weighted. The weight column decides how much weight is added at each time (this can take a negative value to decrease the weight). 

Attribute files are read as follows:\cr
0 1 3 \cr
0 3 2 \cr
1 3 3 \cr
where the first row refers to node 1, the second row to node 2, etc. The first column refers to the first attribute, second column to the second attribute and so on.

A big thank you to the igraph guys as this package relies on their work for many of the more computational tasks!
}
\references{ http://opsahl.co.uk/tnet/ }
\author{ Tore Opsahl; http://toreopsahl.com }
%\keyword{ networks }
\examples{
# Generate a random weighted graph
rg <- rg_w(nodes=100,arcs=300,directed=TRUE)

# Calculate clustering coefficient
clustering_w(rg)
}
