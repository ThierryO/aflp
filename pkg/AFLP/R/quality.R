setGeneric("quality", function(x, which = c("all", "marker", "specimen", "overall")) {
	standardGeneric("quality")
})
setMethod("quality", signature(x = "AFLP"), function(x, which = c("all", "marker", "specimen", "replicate", "plate", "overall")) {
	which <- match.arg(which)
	if(which == "all"){
		x@Quality
	} else if(which == "marker"){
		x@Quality[["Marker"]]
	} else if(which == "overall"){
		x@Quality[["Overall"]]
	} else if(which == "specimen"){
		x@Quality[["Specimen"]]
	} else if(which == "replicate"){
		x@Quality[["Replicate"]]
	} else if(which == "plate"){
		x@Quality[["Plate"]]
	}
})

setGeneric("quality<-", function(data, which = c("all", "marker", "specimen", "overall"), value){
	standardGeneric("quality<-")
})

setMethod("quality<-", signature(data = "AFLP"), function(data, which = c("all", "marker", "specimen", "replicate", "plate", "overall"), value){
	which <- match.arg(which)
	if(which == "all"){
		data@Quality <- value
	} else if(which == "marker"){
		data@Quality[["Marker"]] <- value
	} else if(which == "overall"){
		data@Quality[["Overall"]] <- value
	} else if(which == "specimen"){
		data@Quality[["Specimen"]] <- value
	} else if(which == "replicate"){
		data@Quality[["Replicate"]] <- value
	} else if(which == "plate"){
		data@Quality[["Plate"]] <- value
	}
	data
})
