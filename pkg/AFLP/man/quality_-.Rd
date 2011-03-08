\name{quality<-}
\Rdversion{1.1}
\alias{quality<-}
\title{
Adds or overwrites quality data
}
\description{
Adds or overwrites quality in an AFLP object.
}
\usage{
quality(data, which) <- value
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
	A character value matching c("all", "marker", "specimen", "overall"). Defaults to "all".
}
}
\value{
	\code{data} is updated with the new quality information
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}
\seealso{
\code{\link{quality}}
}
\examples{
#data(willowN)
#border(willowN, pc = "5", marker = 503) <- Inf
}
\keyword{manip}
