\name{growth_l}
\alias{growth_l}
\title{ Identifies growth mechanisms responsible for tie generation in longitudinal networks }
\description{ This function identifies growth mechanisms responsible for tie generation in longitudinal networks. }
\usage{ growth_l(net, perspective = "actor", effects, window=NULL, 
binary=FALSE, nstrata=10, seed=NULL, regression=TRUE) }

\arguments{
  \item{net}{ A longitudinal network }
  \item{perspective}{ whether an actor or dyadic perspective should be used (i.e., whether the network is directed or undirected). Currently, only the actor perspective is included. }
  \item{effects}{ The effects to be analysed }
  \item{window}{ Whether a window should be used. }
  \item{binary}{ Whether duplicated ties should be removed. }
  \item{nstrata}{ Total number of regression observations for each observed tie (i.e., number of control cases plus 1 for the observed tie). Minimum is 2 in which 1 control case is used for each observed case. }
  \item{seed}{ seed for random generator, set to have reproducable results. }
  \item{regression}{ Whether R should perform the regression or output a regression table. If you want to run multiple regression, it is quicker to output the table, and then run multiple regressions. By outputting the table, it is also possible to add square terms and additional data. }
}
\value{ Returns a regression result or table. }
\references{ Tore Opsahl, Bernie Hogan. Growth mechanisms in continuously-observed networks: Communication in a Facebook-like community. arXiv:1010.2141 }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0}
\examples{
## Load sample data
t <- c('2007-09-12 13:45:00', 
'2007-09-12 13:46:31',
'2007-09-12 13:47:54',
'2007-09-12 13:48:21',
'2007-09-12 13:49:27',
'2007-09-12 13:58:14',
'2007-09-12 13:52:17',
'2007-09-12 13:56:59');
i <- c(1,1,2,3,1,3,1,1);
j <- c(2,3,1,2,4,2,3,4);
w <- c(1,1,1,1,1,1,1,1);
sample <- data.frame(t, i, j, w);

## Run the function
growth_l(sample, effects="indegree", nstrata=2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }
