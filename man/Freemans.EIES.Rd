\name{Freemans.EIES}
\docType{data}
\alias{Freemans.EIES.net.1.n48}
\alias{Freemans.EIES.net.2.n48}
\alias{Freemans.EIES.net.3.n32}
\alias{Freemans.EIES.node.Citations.n32}
\alias{Freemans.EIES.node.Discipline.n32}
\alias{Freemans.EIES.node.Name.n32}
\title{ Freeman's EIES network data }
\description{ Freeman's EIES networks (Freeman, 1979) was the main network used in Wasserman and Faust (1994). This dataset was collected in 1978 and contains three networks of researchers working on social network analysis. The first network contains the personal relationships among 48 of the researchers at the beginning of the study (time 1). The second network is the personal relationship at the end of the study (time 2). In these two networks, all ties have a weight between 0 and 4. 4 represents a close personal friend of the researcher's; 3 represents a friend; 2 represents a person the researcher has met; 1 represents a person the researcher has heard of, but not met; and 0 represents a person unknown to the researcher. The third network is different. It is a matrix with the number of messages sent among 32 of the researchers that used an electronic communication tool (frequency matrix). \cr\cr
There are two pieces of information about each of the 32 researchers that were part of the third network (nodal attributes): the main disciplinary affiliation (1: sociology; 2: anthropology; 3: mathematics or statistics; and 4: others) and the number of citations each researcher had in the Social Science Citation Index in 1978.\cr\cr
See http://toreopsahl.com/datasets/ }
\usage{
Freemans.EIES.net.1.n48
Freemans.EIES.net.2.n48
Freemans.EIES.net.3.n32
Freemans.EIES.node.Name.n32
Freemans.EIES.node.Citations.n32
Freemans.EIES.node.Discipline.n32
}
\format{ The networks are data frames with three columns. The first column is the id of the sender, the second column is the id of the receiver; and the third column is the weight of the tie. The attributes are vectors. }
\references{ Freeman, S.C., Freeman, L.C., 1979. The networkers network: A study of the impact of a new communications medium on sociometric structure. Social Science Research Reports 46. University of California, Irvine, CA.\cr
See http://toreopsahl.com/datasets/ }
\keyword{datasets}