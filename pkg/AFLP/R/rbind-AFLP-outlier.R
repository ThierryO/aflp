rbind.AFLP.outlier <- function(..., deparse.level = 1){
	Args <- list(...)
	oReplicate <- unique(do.call("rbind", lapply(Args, replicates)))
	oSpecimen <- unique(do.call("rbind", lapply(Args, specimens)))
	oMarker <- unique(do.call("rbind", lapply(Args, markers)))
	oResiduals <- unique(do.call("rbind", lapply(Args, residuals)))
	AFLP.outlier(Replicate = oReplicate, Specimen = oSpecimen, Marker = oMarker,
		Residual = oResiduals)
}
