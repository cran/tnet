\name{reinforcement_tm}
\alias{reinforcement_tm}
\title{ Reinforcement_tm }
\description{ The reinforcement_tm-function computes Robin and Alexander's (2004) 4-cycle metric for two-mode networks. }
\usage{reinforcement_tm(net)}
\arguments{
  \item{net}{A two-mode network }
}
\value{Returns the score }
\references{ http://toreopsahl.com/tnet/two-mode-networks/clustering/ }
\author{ Tore Opsahl; http://toreopsahl.com }
\note{ version 1.0.0}
\examples{
## Load sample data
net <- rg_tm(10, 7, 0.4)

## Run the programme
reinforcement_tm(net)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ networks }