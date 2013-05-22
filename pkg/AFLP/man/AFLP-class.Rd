\docType{class}
\name{AFLP-class}
\alias{AFLP-class}
\title{Class "AFLP"}
\description{
  This class is design to hold all relevant information on
  an AFLP project.
}
\section{Objects from the Class}{
  Objects can be created from scratch by
  \code{\link{AFLP}}. But we recommend to start with a set
  of specimens that are randomised. This ensures that a
  sufficient number of specimens are replicated. If the
  specimens are already distributed among the plates use
  \code{\link{as.AFLP}} to convert the positions to an AFLP
  object.
}
\examples{
data(Tilia)
}
\author{
  Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul
  Quataert
}
\seealso{
  \code{\link{AFLP}}, \code{\link{as.AFLP}},
  \code{\link{is.AFLP}}, \code{\link{border}},
  \code{\link{fluorescence}}, \code{\link{hclust}},
  \code{\link{outliers}}, \code{\link{quality}},
  \code{\link{QC}}, \code{\link{princomp}},
  \code{\link{replicates}}, \code{\link{specimens}}
}
\keyword{classes}

