\name{border<-}
\alias{border<-}
\title{Adds or overwrites cut-levels}
\arguments{
  \item{data}{An AFLP object}

  \item{value}{A single number of a numeric vector with the
  new cut-levels. Use \code{Inf} to indicate a monomorph
  marker.}

  \item{pc}{Label of the primer combination}

  \item{marker}{Label of the marker}
}
\value{
  \code{data} is updated with the new border information
}
\description{
  Adds or overwrites cut-levels in an AFLP object.
}
\examples{
data(Tilia)
 border(Tilia, pc = "PC1", marker = 116) <- Inf
}
\author{
  Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul
  Quataert
}
\seealso{
  \code{\link{border}}
}
\keyword{manip}

