\name{clustering_w_barrat}
\alias{clustering_w_barrat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Barrat et al. (2004) generalised local clusering coefficient }
\description{
  This programme provides the outcomes of Barrat et al. (2004) generalised local clusering coefficient. See http://toreopsahl.com/2009/01/23/weighted-local-clustering-coefficient/ for a detailed description. By default it measure the triplet value as the average of the two ties; however it can also define it differently. See the blog post.
}
\usage{clustering_w_barrat(edgelist, measure = "am")}
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
\references{ t.opsahl@qmul.ac.uk }
\author{ Tore Opsahl }
\note{ version 1.0.0
}
\examples{
## Generate a random graph
#density: 300/(100*99)=0.03030303; 
#this should be average from random samples
rg <- rg_w(nodes=100,arcs=300,max.weight=10,directed=FALSE)

## Run clustering function
clustering_w_barrat(rg)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }