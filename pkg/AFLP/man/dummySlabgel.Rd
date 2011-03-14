\name{dummySlabgel}
\Rdversion{1.1}
\alias{dummySlabgel}
\title{
Simulate some data for a slab gel.
}
\description{
Simulate some data for a slab gel.
}
\usage{
dummySlabgel(nSpecimen = 50, nGroup = 2, nMarker = 50,
	nReplicate = 3,
	markerProb = c(Monomorph = 0.2, Group = 0.5),
	fixed = c(Intercept = 9, Score = 1.7, MarkerTrend = -0.007),
	betaShape = c(0.3, 0.8),
	VarCov = list(
		Specimen = matrix(c(0.080, -0.017, -0.017, 0.025), ncol = 2), 
		Replicate = matrix(c(0.116, -0.059, -0.059, 0.043), ncol = 2), 
		Marker = matrix(c(0.581, -0.236, -0.236, 0.326), ncol = 2), 
		Plate = matrix(c(0.154, -0.035, -0.035, 0.027), ncol = 2), 
		Noise = 0.292
	),
	transformation = c("log", "logit", "none"))
}
\arguments{
  \item{nSpecimen}{
	The number of specimens. Defaults to 50. 
}
  \item{nGroup}{
	Number of a priori groups in the dataset. Defaults to 2.
}
  \item{nMarker}{
	Number of markers in the dataset. Defaults to 50.
}
  \item{nReplicate}{
	The number of specimens that are replicated on each plate. Defaults to 3.
}
  \item{markerProb}{
	A named vector of length 2 with the probability that a marker is monomorph and the 
	probability that a marker has a different proportion of presence between groups.
	Defaults to c(Monomorph = 0.2, Group = 0.5).
}
  \item{fixed}{
	Overall mean fluorescence, overall difference in magnitude between present and absent bands 
	and trend in fluorescence along the Defaults to c(Intercept = 9, Score = 1.7, MarkerTrend = -0.008).
}
  \item{betaShape}{
	Shapeparameters of a beta distribution used to generate the probability that a marker is 
	present. Defaults to c(0.3, 0.8).
}
  \item{VarCov}{
	A list of variance covariance matrices of the specimens, replicates, markers, plates and 
	noise. Defaults to list(
		Specimen = matrix(c(0.080, -0.017, -0.017, 0.025), ncol = 2), 
		Replicate = matrix(c(0.116, -0.059, -0.059, 0.043), ncol = 2), 
		Marker = matrix(c(0.581, -0.236, -0.236, 0.326), ncol = 2), 
		Plate = matrix(c(0.154, -0.035, -0.035, 0.027), ncol = 2), 
		Noise = 0.292
	).
}
  \item{transformation}{
	Which transformation to use on the raw fluorescence data. Valid choises are "log", "logit" 
	and "none". Defaults to "log". "log" implies the use of log(), hence no zero fluorescences 
	are allowed. With "logit", the raw fluorescence is first devided by the smallest power of 2,
	which is still larger than the largest raw fluorescence. Then a logit transformation is 
	applied (\code{log(p/(1 - p))}). Defaults to "log".
}
}
\value{
An AFLP object with dummy fluoresence data. The fluorescence slot has an extra binary variable 
"Truth".
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}
\seealso{
\code{\link{randomiseSlabgel}}, \code{\link{AFLP}}
}
\examples{
	dummy <- dummySlabgel()
}
\keyword{design}
