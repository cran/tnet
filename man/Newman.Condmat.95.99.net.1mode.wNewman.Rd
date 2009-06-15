\name{Newman.Condmat.95.99.net.1mode.wNewman}
\docType{data}
\alias{Newman.Condmat.95.99.net.1mode.wNewman}
\title{ Newman's condmat 95-99 network (one-mode structure) }
\description{ This is the co-authorship network of scientists based on preprints posted to Condensed Matter section of arXiv E-Print Archive between 1995 and 1999. \cr\cr
This network can be classified as a two-mode or affiliation network since there are two types of "nodes" (authors and papers) and connections exist only between different types of nodes. An author is connected to a paper if her or his name appeared on it. \cr\cr
Few network measures exist for two-mode networks, and therefore, these networks are often projected onto a one-mode (only one type of nodes) network by selecting one of the types of nodes and linking two nodes if they were connected to the same node (of the other kind). \cr\cr
Traditionally, the ties in projected one-mode networks do not have weights. Recent empirical studies of two-mode networks has created a weighted network by defining the weights as the number of co-occurrences (e.g., the number of papers that two authors had collaborated on). \cr\cr
This method was refined by Newman (2001). He argued that smaller collaborations created stronger social bonds among scientists than larger ones. Therefore, he extended this procedure and proposed to define weights among the nodes use the following formula:\cr
w_ij = sum_p 1/(N_p -1)\cr
where w_ij is the weight between node i and node j, p is the papers that they have collaborated on, and N_p is the number of authors on a paper. This implies that if two authors only write a single paper together with no other co-authors, they get a weight of 1. However, if they have a co-author, the weight on the tie between them is 0.5. If two authors have written two papers together without any co-author, the weight of their tie would be 2. A more complicated example is the tie between two authors who have written two papers together: one without any other co-author and one with one co-author. The first paper would give their tie a weight of 1, and the second tie would add 0.5 to the weight of this tie. Therefore, the weight is 1.5.\cr
Note: This method has been explained in more detail in the following post: \cr
http://toreopsahl.com/2009/05/01/projecting-two-mode-networks-onto-weighted-one-mode-networks/ \cr\cr
This is the one-mode network where the weights are defined by Newman's (2001) method.
See http://toreopsahl.com/datasets/ }
\usage{Newman.Condmat.95.99.net.1mode.wNewman}
\format{ A data frame with three columns. The first is the id of the sender; the second is the id of the receiver; and the third is the weight of the tie. }
\references{ Newman, M. E. J., 2001. The structure of scientific collaboration networks. Proceedings of the National Academy of Sciences of the United States of America 98, 404-409.\cr
See http://toreopsahl.com/datasets/ }
\keyword{datasets}