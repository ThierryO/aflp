#'Combine the information of several AFLP.outlier objects
#'
#'Combine the information of several AFLP.outlier objects
#'
#'
#'@param \dots Used to put two or more AFLP.outlier objects.
#'@param deparse.level See \code{\link[base]{rbind}}
#'@return An new AFLP.outlier object combining the information of all objects.
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link[base]{rbind}}
#'@keywords utilities methods
#'@examples
#'
#'  data(Tilia)
#'  extraOutliers <- rbind.AFLP.outlier(outliers(Tilia), outliers(Tilia))
#'@export
rbind.AFLP.outlier <- function(..., deparse.level = 1){
	Args <- list(...)
	oReplicate <- unique(do.call("rbind", lapply(Args, replicates)))
	oSpecimen <- unique(do.call("rbind", lapply(Args, specimens)))
	oMarker <- unique(do.call("rbind", lapply(Args, markers)))
	oResiduals <- unique(do.call("rbind", lapply(Args, residuals)))
	AFLP.outlier(Replicate = oReplicate, Specimen = oSpecimen, Marker = oMarker,
		Residual = oResiduals)
}
