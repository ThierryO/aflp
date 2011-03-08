setGeneric("addOutliers", function(data, extraOutliers){
	standardGeneric("addOutliers")
})
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
