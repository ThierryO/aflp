#'Outliers of an AFLP object
#'
#'This class is designed to hold the outliers of an AFLP object
#'
#'
#'@name AFLP.outlier-class
#'@docType class
#'@section Objects from the Class: Use \code{AFLP.outlier())} to create objects
#'of this class.
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{AFLP.outlier}}, \code{\link{markers}},
#'\code{\link{replicates}}, \code{\link{residuals}}, \code{\link{specimens}}
#'@keywords classes
#'@examples
#'
#'AFLP.outlier()
#'
#'@exportClass AFLP.outlier

setClass("AFLP.outlier",
	representation = representation(Replicate = "data.frame", Specimen = "data.frame",
		Marker = "data.frame", Residual = "data.frame"),
	prototype = prototype(Replicate = data.frame(PC = character(), Replicate = character(), Observed = numeric()),
		Specimen = data.frame(PC = character(), Specimen = character(), Observed = numeric()),
		Marker = data.frame(PC = character(), Marker = numeric(), Observed = numeric()),
		Residual = data.frame(PC = character(), Replicate = character(), Marker = numeric(), Observed = numeric()))
)



#'Creates an AFLP outlier object
#'
#'Creates an AFLP outlier object. The outlier object is used to mark
#'observations as outliers in an AFLP object. These outliers will be omitted
#'when normalising, classifying or calculating the repeatability.
#'
#'
#'@param Replicate A \code{data.frame} with three columns: PC (primer
#'combination), Replicate and Observed.
#'@param Specimen A \code{data.frame} with three columns: PC (primer
#'combination), Specimen and Observed.
#'@param Marker A \code{data.frame} with three columns: PC (primer
#'combination), Marker and Observed.
#'@param Residual A \code{data.frame} with four columns: PC (primer
#'combination), Replicate, Marker and Observed.
#'@return An AFLP outlier object
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{AFLP.outlier-class}}, \code{\link{markers}},
#'\code{\link{replicates}}, \code{\link{residuals}}, \code{\link{specimens}}
#'@keywords classes
#'@examples
#'
#'AFLP.outlier()
#'AFLP.outlier(
#'  Replicate = data.frame(
#'    PC = "PC1", 
#'    Replicate = c("C.09.1744", "C.09.1745"), 
#'    Observed = numeric(2)
#'  )
#')
#'@export
AFLP.outlier <- function(
		Replicate, 
		Specimen, 
		Marker, 
		Residual){
  if(missing(Replicate)){
    Replicate <- data.frame(PC = character(), Replicate = character(), Observed = numeric())
  }
  if(missing(Specimen)){
    Specimen <- data.frame(PC = character(), Specimen = character(), Observed = numeric())
  }
  if(missing(Marker)){
  	Marker <- data.frame(PC = character(), Marker = numeric(), Observed = numeric())
  }
  if(missing(Residual)){
		Residual <- data.frame(PC = character(), Replicate = character(), Marker = numeric(), Observed = numeric())
  }
  return(new("AFLP.outlier", Replicate = Replicate, Specimen = Specimen, Marker = Marker, Residual = Residual))
}
