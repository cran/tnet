\name{Davis.Southern.women}
\docType{data}
\alias{Davis.Southern.women.2mode}
\alias{Davis.Southern.women.1mode.Cooccurance}
\alias{Davis.Southern.women.1mode.Newman}
\title{ Davis' Southern Women network }
\description{
This dataset was collected by Davis and colleague in the 1930s. It contains the observed attendance by 18 Southern women (primary nodes) at 14 social events (secondary nodes). This has been projected onto a co-occurance one-mode network, and a one-mode network based on Newman's (2001) method. }
\usage{
Davis.Southern.women.2mode
Davis.Southern.women.1mode.Cooccurance
Davis.Southern.women.1mode.Newman
}
\format{ The two-mode network is a data frame with two columns (primary nodes and secondary nodes, respectively). The one-mode networks are data frames with three columns: the first column is the id of the sender; the second column is the id of the receiver; and the column third is the weight of the tie. }
\references{ Davis, A., Gardner, B. B., Gardner, M. R., 1941. Deep South. University of Chicago Press, Chicago, IL.\cr
http://toreopsahl.com/datasets/}
\author{ Tore Opsahl; http://toreopsahl.com }
\keyword{datasets}