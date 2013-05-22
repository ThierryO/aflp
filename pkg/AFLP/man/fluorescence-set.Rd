\name{fluorescence<-}
\alias{fluorescence<-}
\title{Adds or overwrites the fluorescence data}
\arguments{
  \item{data}{An AFLP object}

  \item{value}{The new fluorescence data}
}
\value{
  \code{data} is updated with the new fluorescence data
}
\description{
  Adds or overwrites the fluorescence in an AFLP object.
}
\examples{
data(Tilia)
 Fl <- fluorescence(Tilia)
 Fl$Fluorescence[10] <- NA
 fluorescence(Tilia) <- Fl
}
\author{
  Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul
  Quataert
}
\seealso{
  \code{\link{fluorescence}}
}
\keyword{manip}

