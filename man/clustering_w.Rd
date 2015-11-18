\name{clustering_w}
\alias{clustering_w}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Generalised clusering coefficient }
\description{ This function calculates the generalised clusering coefficient as proposed by Opsahl, T., Panzarasa, P., 2009. Clustering in weighted networks. Social Networks 31 (2), 155-163, doi: 10.1016/j.socnet.2009.02.002\cr
Note: If you are having problems with this function (i.e., run out of memory or it being slow for simulations), there is a quicker and much more memory efficient c++ function. However, this function is not fully integrated in R, and requires a few extra steps. Send me an email to get the source-code and Windows-compiled files.  }
\usage{clustering_w(net, measure = "am")}
\arguments{
  \item{net}{A weighted edgelist}
  \item{measure}{ The measure-switch control the method used to calculate the value of the triplets.\cr
  am implies the arithmetic mean method (default)\cr
  gm implies the geometric mean method\cr
  mi implies the minimum method\cr
  ma implies the maximum method\cr
  bi implies the binary measure\cr
  This can be c("am", "gm", "mi", "ma", "bi") to calculate all. }
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