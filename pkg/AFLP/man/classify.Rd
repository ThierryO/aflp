\name{classify}
\Rdversion{1.1}
\alias{classify}
\title{
Classifies normalised AFLP data
}
\description{
The normalised fluorescence score of AFLP data are converted into one or more
classes. The classification is based on the first and second derivative of the
density of the normalised fluorescence. Hence the data must be normalised prior
to classification.

Monomorph markers will have only one class and the corresponding border with have
\code{Inf} as cut-level. The lowest class in polymorphic markers is considered an
'absent' locus, all other classes indicate a 'present' locus.

We strongly suggest that the user thoroughly inspects the resulting plots as the
automated classification might yield some unwanted artifacts. Correct them with
the \code{link{border<-}} function.
}
\usage{
classify(data, output = c("screen", "none", "tex"), 
  maxBorder = 1, tresholdPeakRatio = 0.03, 
  tresholdMonomorph = 0, tresholdMaxValley = 0.95, device = "pdf",
  path = NULL, nrows = 4, ncols = 4, keep.border = FALSE)
}
\arguments{
  \item{data}{
	A normalised AFLP object. Unnormalised object will be normalised prior to 
	classification.
}
  \item{output}{
	Indicates how the inspections plots should be given. "none" suppresses them,
	"screen" prints them on the screen, "tex" save them to a file and generates
	LaTeX to include these files into a document.
}
  \item{maxBorder}{
	The maximum number of borders per marker. 
}
  \item{tresholdPeakRatio}{
	The minimum height of a potential peak in the density relative to the maximum
	density. Peak with a ratio lower that this threshold are not considered as
	peaks. Defaults to 0.03.
}
  \item{tresholdMonomorph}{
	Calculate the ratio of 'absent' and 'present' bins in a marker. Consider the
	marker as monomorph if either of them is smaller than the threshold. Defaults to 0. 
}
  \item{tresholdMaxValley}{
   A treshold for valleys between peaks in the relative density. P,my vallyes below this treshold are considered for breaks. Defaults to 0.95.
  }
  \item{path}{
	the path where the figures are saved. Only used if \code{output = "tex"}. 
	Defaults to NULL, which is the working directory.
}
  \item{device}{
	the device to which the figures are saved. See \code{\link[ggplot2]{ggsave}}
	for the available devices. Only used if \code{output = "tex"}. Defaults to "pdf".
}
  \item{nrows}{
	Gives the prefer number of rows with plot per figure. Defaults to 4.
}
  \item{ncols}{
	Gives the prefer number of columns with plot per figure. Defaults to 4.
}
  \item{keep.border}{
	Logical. If FALSE then the borders are recalculated and overwritten. If TRUE
	the borders in the object are used for the classification. The latter is 
	intended to update the classification after manually altering the borders.
	Default to FALSE.
}
}
\value{
Returns the \code{data} object with a updated \code{borders} slot and updated
\code{Score} in the \code{Fluorescence} slot.
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}
\seealso{
\code{\link{border<-}}, \code{\link{border}}, \code{\link{normalise}}, \code{\link[ggplot2]{ggsave}}
}
\examples{
  data(Tilia)
  Tilia <- classify(Tilia, output = "none")
}
\keyword{manip}
\keyword{classif}

