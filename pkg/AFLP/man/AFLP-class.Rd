\name{AFLP-class}
\Rdversion{1.1}
\docType{class}
\alias{AFLP-class}

\title{Class "AFLP"}
\description{This class is design to hold all relevant information on an AFLP project.}
\section{Objects from the Class}{
Objects can be created from scratch by \code{\link{AFLP}}. But we recommend to 
start with a set of specimens that are randomised. This ensures that a sufficient number of 
specimens are replicated. If the specimens are already distributed among the 
plates use \code{\link{as.AFLP}} to convert the positions to an AFLP object. 
}
\section{Slots}{
	 \describe{
	\item{\code{Specimens}:}{A \code{"data.frame"} containing at least the variables Specimen 
		and Group. Specimen indicates the label of sampled individual. Group is a grouping
		variable indicating the a priori clustering of the specimens.}
    \item{\code{Replicates}:}{A \code{"data.frame"} indicating which specimen is 
    	loaded on a given lane, capilar and plate. Each combination of plate, capilar and lane has a
    	unique replicate label.}
	\item{\code{QC}:}{A list with elements Specimens and Replicates. The \code{"data.frame"}
		in each element indicate the specimens and replicates which are used for external
		quality control. These samples are not used in the analysis of the data}
    \item{\code{Fluorescence}:}{A \code{"data.frame"} containing the fluorescence
    	for each bin.}
    \item{\code{model}:}{A \code{"formula"} indicating the model that is used for
    	normalising the fluorescence data}
    \item{\code{outliers}:}{Object of class \code{"AFLP.outlier"} containing the
    	eliminated observations due to outliers}
    \item{\code{Borders}:}{A \code{"data.frame"} containing the cut-levels for
    	each marker}
    \item{\code{Quality}:}{A \code{"list"} containing five data.frames with the quality of the
    markers, specimens, replicates, plates and the overall quality. The quality is based on the scores of the 
    specimens with multiple replicates.}
  }
}
\section{Methods}{
  \describe{
    \item{border}{extract the borders}
    \item{border<-}{overwrite the borders of a given marker}
    \item{fluorescence}{extract the fluorescence data}
    \item{hclust}{perform hierarchical clustering on the AFLP object}
    \item{outliers}{extract the outliers}
    \item{princomp}{perform a PCO analysis on the AFLP object}
    \item{replicates}{extract the data.frame of replicates}
    \item{specimens}{extract the data.frame of specimens}
    \item{QC}{extract the QC samples}
    \item{QC<-}{replace the QC samples}
    \item{quality}{extract the quality data}
    \item{quality<-}{overwrite the quality data}
	 }
}
\author{Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert}
\seealso{
	\code{\link{AFLP}}, \code{\link{as.AFLP}}, \code{\link{is.AFLP}}, \code{\link{border}}, 
	\code{\link{fluorescence}}, \code{\link{hclust}}, \code{\link{outliers}}, \code{\link{quality}},
	\code{\link{QC}}, \code{\link{princomp}}, \code{\link{replicates}}, 
	\code{\link{specimens}}
}
\examples{
#data(willow)
#willow
}
\keyword{classes}
