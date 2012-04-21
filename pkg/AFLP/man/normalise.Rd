\name{normalise}
\Rdversion{1.1}
\alias{normalise} 
\title{
Perform a normalisation (calibration) procedure on the observed fluorescence.
}
\description{
Calculates normalised fluorescence values by taking into account the differences
between replicates nested in specimens, plates and markers. This is based on a 
linear mixed model. The form of the model is automatically chosen based on the 
number of replicates, specimens, plates and marker. The same form is apply on 
each primer combination separately.
}
\usage{
normalise(data, output = c("screen", "tex", "none"), path = NULL, device = "pdf", SpecimenEffect = FALSE, level = 0.99, transformation = c("log", "logit", "none"))
}
\arguments{
  \item{data}{
	An AFLP object
}
  \item{output}{
	Which output is required. "screen" put QQ-plots of the random effects, QQ-plots
	of the residuals and possible outliers on the screen. "tex" givens the same
	information but saves the QQ-plots to files and report LaTeX code to include
	the information in a LaTeX document.
}
  \item{path}{
	the path where the figures are saved. Only used if \code{output = "tex"}. 
	Defaults to NULL, which is the working directory.
}
  \item{device}{
	the device to which the figures are saved. See \code{\link[ggplot2]{ggsave}}
	for the available devices. Only used if \code{output = "tex"}. Defaults to "pdf".
}
  \item{SpecimenEffect}{
	Add a random effect of the specimens to the model. Defaults to FALSE.
}
  \item{level}{
	The level of the prediction intervals. Used to determine possible outliers in
	the QQ-plots. Defaults to 0.99.
}
  \item{transformation}{
	Which transformation to use on the raw fluorescence data. Valid choises are "log", "logit" 
	and "none". Defaults to "log". "log" implies the use of log(), hence no zero fluorescences 
	are allowed. With "logit", the raw fluorescence is first devided by the smallest power of 2,
	which is still larger than the largest raw fluorescence. Then a logit transformation is 
	applied (\code{log(p/(1 - p))}).
}
}
\value{
\item{data}{The altered data object that was put into the function. Outliers in
	the \code{outliers} slot are removed from the \code{Fluorescence} slot. The 
	Normalised and Score columns from the \code{Fluorescence} slot are overwritten.
	Normalised holds the new normalised values. Score with be fill with NA.}
\item{outliers}{An AFLP.outlier object with all possible outliers detected by this
	procedure.}
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}
\seealso{
\code{\link{classify}}, \code{\link[ggplot2]{ggsave}}
}
\examples{
  data(TiliaDesign)
  Tilia <- as.AFLP(TiliaDesign)
  Tilia <- readSAGA(system.file("extdata", "Tilia_bandvaluespc1", package = "AFLP"), add.to = Tilia)
  Tilia <- readSAGA(system.file("extdata", "Tilia_bandvaluespc2", package = "AFLP"), add.to = Tilia)
  Tilia <- readSAGA(system.file("extdata", "Tilia_bandvaluespc3", package = "AFLP"), add.to = Tilia)
  Tilia <- readSAGA(system.file("extdata", "Tilia_bandvaluespc4", package = "AFLP"), add.to = Tilia)
  Tilia <- clean(Tilia)
  normalise(Tilia, output = "none")
}
\keyword{models}
\keyword{regression}
\keyword{design}
