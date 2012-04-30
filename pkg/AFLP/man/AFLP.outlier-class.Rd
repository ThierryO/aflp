\name{AFLP.outlier-class}
\Rdversion{1.1}
\docType{class}
\alias{AFLP.outlier-class}

\title{Outliers of an AFLP object}
\description{This class is designed to hold the outliers of an AFLP object}
\section{Objects from the Class}{
Use \code{AFLP.outlier())} to create objects of this class.
}
\section{Slots}{
	 \describe{
    \item{\code{Replicate}:}{A \code{"data.frame"} with three columns: PC, 
    	Replicate and Observed. Indicate which Replicate of a given primer 
    	combination (PC) is an outlier.}
    \item{\code{Specimen}:}{A \code{"data.frame"} with three columns: PC, 
    	Specimen and Observed. Indicate which Specimen of a given primer 
    	combination (PC) is an outlier.}
    \item{\code{Marker}:}{A \code{"data.frame"} with three columns: PC, 
    	Marker and Observed. Indicate which Marker of a given primer 
    	combination (PC) is an outlier.}
    \item{\code{Residual}:}{A \code{"data.frame"} with four columns: PC, 
    	Replicate, Marker and Observed. Indicate which Replicate of a given 
    	primer combination (PC) and Marker is an outlier.}
  }
}
\section{Methods}{
  \describe{
    \item{markers}{extracts the data.frame with marker outliers}
    \item{replicates}{extract the data.frame with replicate outliers}
    \item{residuals}{extract the data.frame with residual outliers}
    \item{specimens}{extract the data.frame with specimen outliers}
	 }
}
\author{Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert}
\seealso{
\code{\link{AFLP.outlier}}, \code{\link{markers}}, \code{\link{replicates}}, \code{\link{residuals}}, \code{\link{specimens}}
}
\examples{
AFLP.outlier()
}
\keyword{classes}
