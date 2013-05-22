\name{clean}
\alias{clean}
\title{Does some rudimentary cleaning of an AFLP object.}
\usage{
  clean(data)
}
\arguments{
  \item{data}{An AFLP object}
}
\value{
  A cleaned version of \code{data}
}
\description{
  Does some rudimentary cleaning of an AFLP object. Mainly
  used to do get a clean object prior to normalisation.
}
\examples{
data(TiliaDesign)
 TiliaC <- as.AFLP(TiliaDesign)
 TiliaC <- readSAGA(
   system.file("extdata", "Tilia_bandvaluespc1", package = "AFLP"),
   add.to = TiliaC)
 TiliaC <- readSAGA(
   system.file("extdata", "Tilia_bandvaluespc2", package = "AFLP"),
   add.to = TiliaC)
 TiliaC <- readSAGA(
   system.file("extdata", "Tilia_bandvaluespc3", package = "AFLP"),
   add.to = TiliaC)
 TiliaC <- readSAGA(
   system.file("extdata", "Tilia_bandvaluespc4", package = "AFLP"),
   add.to = TiliaC)
 clean(TiliaC)
}
\author{
  Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul
  Quataert
}
\seealso{
  \code{\link{normalise}}
}
\keyword{manip}

