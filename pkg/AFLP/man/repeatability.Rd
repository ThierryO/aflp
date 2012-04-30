\name{repeatability}
\Rdversion{1.1}
\alias{repeatability}
\title{
Estimate the repeatability of AFLP data
}
\description{
This function evaluates two indicators for the repeatability of the data: one based
on the fluoresence and one on the classification. The indicators are based on all
specimens with more than one replicate, outliers excluded. The indicators are
given for both the specimens as the markers.

The indicator based on the fluorescence (raw or normalised) behaves like a variance.
0 equals a perfect match between all replicates from the same specimen. Higher
values indicate less repeatable data. There is no upper limit for this value.
The value is only useful to compare specimen or marker with the same project.

The indicator based on the score ranges from 0 (not reproducible at all, score is
more or less random) to 1 (perfect repeatability).

The calculation of both indicator is described in the Details section.
}
\usage{
repeatability(data, output = c("screen", "tex", "none"), 
  bootstrap = FALSE, minMarker = NULL, path = NULL, 
  device = "pdf")
}
\arguments{
  \item{data}{
	An AFLP object with at least raw fluoresence data.
}
  \item{output}{
	Which output is required. "screen" put graphics and possible outliers on the 
	screen. "tex" givens the same information but saves the graphics to files 
	and report LaTeX code to include the information in a LaTeX document. "none"
	suppresses the output.
}
  \item{bootstrap}{
	Logical. Indicates whether a bootstrap procedure is run to detect possible
	outliers. These are specimens or markers which are probably unreliable because
	of a bad repeatability. Default to FALSE. Warning: setting this to TRUE can 
	require a long computing time.
}
  \item{minMarker}{
	All markers and specimens with a score less than \code{minMarker} are put in AFLP.outlier
	object. Defaults to NULL.
}
  \item{path}{
	the path where the figures are saved. Only used if \code{output = "tex"}. 
	Defaults to NULL, which is the working directory.
}
  \item{device}{
	the device to which the figures are saved. See \code{\link[ggplot2]{ggsave}}
	for the available devices. Only used if \code{output = "tex"}. Defaults to "pdf".
}
}
\details{
First a selection is made from all possible combinations of marker and specimen
were data of more than one replicate is available. This selection is used for both
indicators.

The indicator on the fluorescence (raw and normalised) starts by calculating the
variance of the fluorescence for each combination of specimen and marker. If the 
data is repeatable then the fluorescence will be very similar and hence the variance
will be close to zero. The indicator per specimen is simply the mean of these 
variances over all markers. Likewise we the mean per marker of the variances 
over all specimens is the indicator per marker.

The indicator on the score is based on the number of possible mistakes. First the
scores are converted into a binary score. The lowest class is considered 'absent',
all other classed 'present'. Then we look at the number of 'absent' and 'present'
replicates for each combination of marker and specimen. The class with the highest
number is presumed to be the correct class. Hence the maximum number of mistakes
for each combination of marker and specimen is the number of replicates divide by
2 and rounded downward. Now we have for all those combinations a number of mistakes
and the maximum number of mistakes. We calculate for each specimen the sum of both
numbers over all markers. Then we subtract the total number of mistakes from the
total maximum number of mistake and divide that by the total maximum number of 
mistakes. If all replicates yield the same class, then no mistakes are made and
the indicator equals 1. If the data has a very bad repeatability and half of the
replicates are 'absent' and half 'present', then the total number of mistakes
will equal the total maximum number of mistakes. This leads to an indicator equal
to 0. The indicator per marker is calculated in a similar fashion (aggregation on 
marker instead of specimen).
}
\value{
\item{data}{
	An ALFP object were the quality data is appended to.
}
\item{Outliers}{
	An AFLP.outlier object with the possible outliers. This is based on the bootstrap
	procedure. Hence using \code{boostrap = FALSE} will result is an empty AFLP.outlier
	object.
}
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}
\seealso{
\code{\link{normalise}}, \code{\link{classify}}, \code{\link[ggplot2]{ggsave}}
}
\examples{
	data(Tilia)
	output <- repeatability(Tilia, output = "none")
}
\keyword{attribute}
