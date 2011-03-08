\name{QC<-}
\Rdversion{1.1}
\alias{QC<-}
\title{
Adds or overwrites quality data
}
\description{
Adds or overwrites quality control samples in an AFLP object.
}
\usage{
QC(data, which) <- value
}
\arguments{
  \item{data}{
	An AFLP object
}
  \item{value}{
	When which == "all", value must be a list with quality information. Otherwise a data.frame
	with the corresponding quality information.
}
  \item{which}{
	A character value matching c("all", "specimen", "replicate"). Defaults to "all".
}
}
\value{
	\code{data} is updated with the new quality information
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}
\seealso{
\code{\link{QC}}
}
\examples{
#data(willowN)
#border(willowN, pc = "5", marker = 503) <- Inf
}
\keyword{manip}
