\name{Cross.Parker.Manufacturing.net.info}
\docType{data}
\alias{Cross.Parker.Manufacturing.net.info}
\alias{Cross.Parker.Manufacturing.net.aware}
\alias{Cross.Parker.Manufacturing.node.location}
\alias{Cross.Parker.Manufacturing.node.orglevel}
\alias{Cross.Parker.Manufacturing.node.tenure}
\title{ Intra-organisational networks }
\description{ This dataset contains two intra-organizational networks from a research team in a manufacturing  company (77 employees). These networks was used by Cross and Parker (2004). \cr\cr
In the first network, the ties among the researchers are differentiated in terms of advice ("Please indicate the extent to which the people listed below provide you with information you use to accomplish your work"). The weights are based on the following scale: 0: I Do Not Know This Person/I Have Never Met this Person; 1: Very Infrequently; 2: Infrequently; 3: Somewhat Infrequently; 4: Somewhat Frequently; 5: Frequently; and 6: Very Frequently.\cr\cr
The second network is based on the employees' awareness of each others' knowledge and skills ("I understand this person's knowledge and skills. This does not necessarily mean that I have these skills or am knowledgeable in these domains but that I understand what skills this person has and domains they are knowledgeable in"). The weight scale in this network is: 0: I Do Not Know This Person/I Have Never Met this Person; 1: Strongly Disagree; 2: Disagree; 3: Somewhat Disagree; 4: Somewhat Agree; 5: Agree; and 6: Strongly Agree.\cr\cr
In addition to the relational data, the dataset also contains information about the people (nodal attributes). The following attributes are known: location (1: Paris; 2: Frankfurt; 3: Warsaw; 4: Geneva), tenure (1: 1-12 months; 2: 13-36 months; 3: 37-60 months; 4: 61+ months) and the organisational level (1: Global Dept Manager; 2: Local Dept Manager; 3: Project Leader; 4: Researcher). \cr\cr
See http://toreopsahl.com/datasets/ }
\usage{
Cross.Parker.Manufacturing.net.info
Cross.Parker.Manufacturing.net.aware
Cross.Parker.Manufacturing.node.location
Cross.Parker.Manufacturing.node.orglevel
Cross.Parker.Manufacturing.node.tenure
}
\format{ The networks are data frames with three columns. The first column is the id of the sender; the second column is the id of the receiver; and the third column is the weight of the tie. The nodal attributes are vectors. }
\references{ Cross, R., Parker, A., 2004. The Hidden Power of Social Networks. Harvard Business School Press, Boston, MA.\cr
http://toreopsahl.com/datasets/ }
\keyword{datasets}