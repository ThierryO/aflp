#'Extract the quality data from an AFLP object
#'
#'Extracts the quality from an AFLP object
#'
#'Extracts the quality data of an AFLP object
#'
#'Extracts the quality of an AFLP object
#'
#'
#'@param x An AFLP object
#'@param which Which quality information to extract. Must be one of c("all",
#'"marker", "specimen", "replicate", "plate", "overall"). Defaults to "all".
#'@return In case when which == "all" a list with 3 data.frame containing the
#'quality information.  Otherwise a data.frame with the selected quality data.
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@export
setGeneric("quality", function(x, which = c("all", "marker", "specimen", "replicate","plate", "primercombination", "global")) {
	standardGeneric("quality")
})

#'Extract the quality data from an AFLP object
#'
#'Extracts the quality from an AFLP object
#'
#'@name quality-method
#'@aliases quality-method quality,AFLP-method
#'@docType methods
#'@section Methods: \describe{
#'
#'\item{data = "AFLP"}{Extracts the quality data from an AFLP object} }
#'@keywords methods attribute
#'@exportMethod quality
setMethod("quality", signature(x = "AFLP"), function(x, which = c("all", "marker", "specimen", "replicate", "plate", "primercombination", "global")) {
	which <- match.arg(which)
	if(which == "all"){
		x@Quality
	} else if(which == "marker"){
		x@Quality[["Marker"]]
	} else if(which == "primercombination"){
		x@Quality[["Primercombination"]]
	} else if(which == "global"){
		x@Quality[["Global"]]
	} else if(which == "specimen"){
		x@Quality[["Specimen"]]
	} else if(which == "replicate"){
		x@Quality[["Replicate"]]
	} else if(which == "plate"){
		x@Quality[["Plate"]]
	}
})

#'Extracts the quality data of an AFLP object
#'
#'Extracts the quality of an AFLP object
#'
#'
#'@param x An AFLP object
#'@param which Which quality information to extract. Must be one of c("all",
#'"marker", "specimen", "replicate", "plate", "overall"). Defaults to "all".
#'@return In case when which == "all" a list with 3 data.frame containing the
#'quality information.  Otherwise a data.frame with the selected quality data.
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{quality-method}}
#'@keywords attribute
#'@examples
#'
#'  data(Tilia)
#'  qc <- quality(Tilia)
#'  qcm <- quality(Tilia, "marker")
#'  qcs <- quality(Tilia, "specimen")
#'  qcr <- quality(Tilia, "replicate")
#'  qcp <- quality(Tilia, "plate")
#'  qcpc <- quality(Tilia, "primercombination")
#'  qcg <- quality(Tilia, "global")
#'@exportMethod quality<-
setGeneric("quality<-", function(data, which = c("all", "marker", "specimen", "replicate", "plate", "primercombination", "global"), value){
	standardGeneric("quality<-")
})

#'Adds or overwrites quality data
#'
#'Adds or overwrites quality in an AFLP object.
#'
#'@name quality<--methods
#'@aliases quality<--methods quality<-,AFLP-method
#'
#'@param data An AFLP object
#'@param value When which == "all", value must be a list with quality
#'information. Otherwise a data.frame with the corresponding quality
#'information.
#'@param which A character value matching c("all", "marker", "specimen",
#'"replicate", "plate", "primercombinations", "global"). Defaults to "all".
#'@return \code{data} is updated with the new quality information
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{quality}}
#'@keywords manip
#'@examples
#'
#'  data(Tilia)
#'  qc <- quality(Tilia)
#'  qc$Global$Score <- NA
#'  quality(Tilia) <- qc
#'
#'@exportMethod quality<-
setMethod("quality<-", signature(data = "AFLP"), function(data, which = c("all", "marker", "specimen", "replicate", "plate", "primercombination", "global"), value){
	which <- match.arg(which)
	if(which == "all"){
		data@Quality <- value
	} else if(which == "marker"){
		data@Quality[["Marker"]] <- value
	} else if(which == "primercombination"){
		data@Quality[["Primercombination"]] <- value
	} else if(which == "global"){
		data@Quality[["Global"]] <- value
	} else if(which == "specimen"){
		data@Quality[["Specimen"]] <- value
	} else if(which == "replicate"){
		data@Quality[["Replicate"]] <- value
	} else if(which == "plate"){
		data@Quality[["Plate"]] <- value
	}
	data
})
