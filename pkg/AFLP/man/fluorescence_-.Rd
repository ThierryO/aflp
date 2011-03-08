\name{fluorescence<-}
\Rdversion{1.1}
\alias{fluorescence<-}
\title{
Adds or overwrites the fluorescence data
}
\description{
Adds or overwrites the fluorescence in an AFLP object.
}
\usage{
fluorescence(data) <- value
}
\arguments{
  \item{data}{
	An AFLP object
}
  \item{value}{
	The new fluorescence data
}
}
\value{
	\code{data} is updated with the new fluorescence data
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}
\seealso{
\code{\link{fluorescence}}
}
\examples{
#data(willowN)
#border(willowN, pc = "5", marker = 503) <- Inf
}
\keyword{manip}
