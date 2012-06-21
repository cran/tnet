\name{celegans.n306.net}
\docType{data}
\alias{celegans.n306.net}
\title{ The neural network of the Caenorhabditis elegans worm (c.elegans) }
\description{
This dataset contains the neural network of the Caenorhabditis elegans worm (C.elegans). It was studied by Watts and Strogatz (1998). The network contains 306 nodes that represent neurons. Two neurons are connected if at least one synapse or gap junction exist between them. The weight is the number of synapses and gap junctions. This network was obtained from the Collective Dynamics Group's website.
}
\usage{
celegans.n306.net 
}
\format{ A data frame with three columns. The first is the id of the sender; the second is the id of the receiver; and the third is the weight of the tie. }
\references{ Watts, D. J., Strogatz, S. H., 1998. Collective dynamics of "small-world" networks. Nature 393, 440-442.\cr
http://toreopsahl.com/datasets/}
\author{ Tore Opsahl; http://toreopsahl.com }
\keyword{datasets}