\name{clustering_w_local}
\alias{clustering_w_local}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Barrat et al. (2004) generalised local clusering coefficient }
\description{ This function calculates Barrat et al. (2004) generalised local clusering coefficient.\cr
See http://toreopsahl.com/2009/01/23/weighted-local-clustering-coefficient/ for a detailed description. By default it measure the triplet value as the average of the two ties; however it can also define it differently. See the blog post.\cr
Note: If there are very large tie weights in a network, the geometric method in R fails. However, this can be fixed by transforming the values.\cr
net[,3] <- (net[,3]/min(net[,3])) \cr
This step is not required unless you receive warnings when running the function.
}
\usage{clustering_w_local(net, measure = "am")}
\arguments{
  \item{net}{A weighted edgelist}
  \item{measure}{ The measure-switch control the method used to calculate the value of the triplets.\cr
  am implies the arithmetic mean method\cr
  gm implies the geometric mean method\cr
  mi implies the minimum method\cr
  ma implies the maximum method\cr
  bi implies the binary measures\cr
  This can be c("am", "gm", "mi", "ma", "bi") to calculate all. }
}
\value{ Returns a data.frame with at least two columns: the first containts the node ids of all the nodes in the edgelist, and the remaining ones containt the corresponding clustering scores. }
\references{ Barrat, A., Barthelemy, M., Pastor-Satorras, R., Vespignani, A., 2004. The architecture of complex weighted networks. Proceedings of the National Academy of Sciences 101 (11), 3747-3752. arXiv:cond-mat/0311416 \cr
http://toreopsahl.com/2009/01/23/weighted-local-clustering-coefficient/ }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0
}
\examples{
## Generate a random graph
#density: 300/(100*99)=0.03030303; 
#this should be average from random samples
rg <- rg_w(nodes=100,arcs=300,weights=1:10,directed=FALSE)

## Run clustering function
clustering_w_local(rg)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }