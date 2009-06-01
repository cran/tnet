\name{clustering_w}
\alias{clustering_w}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Generalised clusering coefficient }
\description{
  This programme provides the outcomes of the generalised clusering coefficient as proposed by Opsahl and Panzarasa: Clustering in weighted networks
}
\usage{clustering_w(edgelist, measure = "mi")}
\arguments{
  \item{edgelist}{A weighted edgelist}
  \item{measure}{ The measure-switch control the method used to calculate the value of the triplets.\cr
  am implies the arithmetic mean method\cr
  gm implies the geometric mean method\cr
  mi implies the minimum method\cr
  ma implies the maximum method\cr
  This can be c("am", "gm", "mi", "ma") to calculate all. }
}
\value{Returns the outcome of the equation presented in the paper for the method specific (measure)}
\references{Opsahl. T., Panzarasa, P., 2009. Clustering in weighted networks. Social Networks. }
\author{ Tore Opsahl t.opsahl@qmul.ac.uk }
\note{ version 1.0.0 }

\examples{
## Generate a random graph
#density: 300/(100*99)=0.03030303; 
#this should be average from random samples
rg <- rg_w(nodes=100,arcs=300,max.weight=10)

## Run clustering function
clustering_w(rg)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }