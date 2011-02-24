\name{USairport.n500}
\docType{data}
\alias{USairport.n500.net}
\title{ The network among the 500 busiest US commercial airports. }
\description{ The nodes in this network is the 500 busiest commercial airports in the United States. A tie exists between two airports if a flight was scheduled between them in 2002. The weights corresponds to the number of seats available on the scheduled flights. Even thought this type of networks is directed by nature as a flight is scheduled from one airport and to another, the networks are highly symmetric (Barrat et al., 2004). Therefore, the version of this network is undirected (i.e., the weight of the tie from one airport towards another is equal to the weight of the reciprocal tie). This network was obtained from the Complex Networks Collaboratory's website\cr\cr
See http://toreopsahl.com/datasets/ }
\usage{USairport.n500.net}
\format{ A data frame with three columns. The first two columns are the nodes' ids, and the third column is the weight of the tie. }
\references{ Colizza, V., Pastor-Satorras, R., Vespignani, A., 2007. Reaction-diffusion processes and metapopulation models in heterogeneous networks. Nature Physics 3, 276-282.\cr
See http://toreopsahl.com/datasets/ }
\keyword{datasets}