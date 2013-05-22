\name{QC<-}
\alias{QC<-}
\title{Adds or overwrites quality control samples in an AFLP object.}
\arguments{
  \item{data}{An AFLP object}

  \item{value}{When which == "all", value must be a list
  with quality information. Otherwise a data.frame with the
  corresponding quality information.}

  \item{which}{A character value matching c("all",
  "specimen", "replicate"). Defaults to "all".}
}
\value{
  \code{data} is updated with the new quality information
}
\description{
  Adds or overwrites quality control samples in an AFLP
  object.
}
\examples{
data(Tilia)
 tmp <- replicates(Tilia)
 QC(Tilia, which = "all") <- list(
   Specimen = data.frame(
     Specimen = tmp$Specimen[grep("QC", tmp$Specimen)],
     Type = "method"
   ),
   Replicate = data.frame(
     Replicate = tmp$Replicate[grep("qc", tmp$Replicate)],
     Type = "method"
   )
 )
 QC(Tilia, which = "specimen") <- data.frame(
   Specimen = tmp$Specimen[grep("QC", tmp$Specimen)],
   Type = "method"
 )
 QC(Tilia, which = "replicate") <- data.frame(
   Replicate = tmp$Replicate[grep("qc", tmp$Replicate)],
   Type = "method"
 )
}
\author{
  Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul
  Quataert
}
\seealso{
  \code{\link{QC}}
}
\keyword{manip}

