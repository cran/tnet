\name{clustering_w}
\alias{clustering_w}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Generalised clusering coefficient }
\description{ This function calculates the generalised clusering coefficient as proposed by Opsahl, T., Panzarasa, P., 2009. Clustering in weighted networks. Social Networks 31 (2), 155-163, doi: 10.1016/j.socnet.2009.02.002\cr
Note: If you are having problems with this function (i.e., run out of memory), there is a much more memory efficient c++ function. However, this function is not fully integrated in R, and requires a few extra steps.  }
\usage{clustering_w(net, measure = "mi", subsample=1, seed=NULL)}
\arguments{
  \item{net}{A weighted edgelist}
  \item{measure}{ The measure-switch control the method used to calculate the value of the triplets.\cr
  am implies the arithmetic mean method\cr
  gm implies the geometric mean method\cr
  mi implies the minimum method\cr
  ma implies the maximum method\cr
  bi implies the binary measure\cr
  This can be c("am", "gm", "mi", "ma", "bi") to calculate all. }
  \item{subsample}{Whether a only a subset of triplets should we used when calculating the measure. This is particularly useful when running out of memory analysing large networks. If it is set to 1, all the triplets are analysed. If it set to a value below one, this is roughly the proportion of triplets that will be analysed. If it is set to an interger greater than 1, this number of triplets that will be analysed.}
  \item{seed}{If a subset of triplets is analysed, by setting this parameter, the results are reproducable.}
}
\value{Returns the outcome of the equation presented in the paper for the method specific (measure)}
\references{ Opsahl, T., Panzarasa, P., 2009. Clustering in weighted networks. Social Networks 31 (2), 155-163, doi: 10.1016/j.socnet.2009.02.002 \cr
http://toreopsahl.com/2009/04/03/article-clustering-in-weighted-networks/ }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0 }

\examples{
## Generate a random graph
#density: 300/(100*99)=0.03030303; 
#this should be average from random samples
rg <- rg_w(nodes=100,arcs=300,weights=1:10)

## Run clustering function
clustering_w(rg)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }