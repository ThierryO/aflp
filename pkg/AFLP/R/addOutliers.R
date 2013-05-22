#'Appends outliers to an AFLP object
#'
#'The outliers in the AFLP object are supplemented with the new outliers.
#'Duplicate outliers are ignored.
#'
#'
#'@param data An AFLP object
#'@param extraOutliers An AFLP outlier object
#'@return The \code{data} object is returned, supplemented with the outliers
#'from \code{extraOutliers}
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{addOutliers-methods}}, \code{\link{AFLP-class}},
#'\code{\link{AFLP.outlier-class}}
#'@keywords attribute
#'@examples
#'
#'  data(Tilia)
#'  nOutput <- normalise(Tilia, output = "none")
#'  RemovingAllPossibleOutliers <- addOutliers(Tilia, nOutput$outliers)
#'  RemovingAllPossibleOutlieingReplicates <- 
#'    addOutliers(Tilia, replicates(nOutput$outliers))
#'
#'@export

setGeneric("addOutliers", function(data, extraOutliers){
	standardGeneric("addOutliers")
})

#'Add outliers to an AFLP object
#'
#'Add outliers to an AFLP object
#'
#'
#'@name addOutliers-methods
#'@aliases addOutliers-methods addOutliers,AFLP,AFLP.outlier-method
#'addOutliers,AFLP,data.frame-method
#'@docType methods
#'@section Methods: \describe{
#'
#'\item{data = "AFLP", extraOutliers = "AFLP.outlier"}{Add outliers as an
#'AFLP.outlier object to an AFLP object}
#'
#'\item{data = "AFLP", extraOutliers = "data.frame"}{Add outliers as a
#'data.frame to an AFLP object} }
#'@keywords methods manip
#'@export

setMethod("addOutliers", signature(data = "AFLP", extraOutliers = "AFLP.outlier"), function(data, extraOutliers) {
	data@outliers <- rbind.AFLP.outlier(outliers(data), extraOutliers)
	return(data)
})

setMethod("addOutliers", signature(data = "AFLP", extraOutliers = "data.frame"), function(data, extraOutliers) {
	if(!"PC" %in% colnames(extraOutliers)){
		stop("The data.frame must contain a column PC")
	}
	if(!"Observed" %in% colnames(extraOutliers)){
		extraOutliers$Observed <- NA
	}
	if("Replicate" %in% colnames(extraOutliers)){
		if("Marker" %in% colnames(extraOutliers)){
			newOutliers <- AFLP.outlier(Residual = extraOutliers[, c("PC", "Replicate", "Marker", "Observed")])
		} else {
			newOutliers <- AFLP.outlier(Replicate = extraOutliers[, c("PC", "Replicate", "Observed")])
		}
	} else if ("Specimen" %in% colnames(extraOutliers)){
		newOutliers <- AFLP.outlier(Specimen = extraOutliers[, c("PC", "Specimen", "Observed")])
	} else if ("Marker" %in% colnames(extraOutliers)){
		newOutliers <- AFLP.outlier(Marker = extraOutliers[, c("PC", "Marker", "Observed")])
	} else {
		stop("data.frame must contain at least a Replicate, Specimen or Marker column")
	}
	data@outliers <- rbind.AFLP.outlier(outliers(data), newOutliers)
	return(data)
})
