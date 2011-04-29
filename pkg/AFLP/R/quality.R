setGeneric("quality", function(x, which = c("all", "marker", "specimen", "replicate","plate", "primercombination", "global")) {
	standardGeneric("quality")
})
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

setGeneric("quality<-", function(data, which = c("all", "marker", "specimen", "replicate", "plate", "primercombination", "global"), value){
	standardGeneric("quality<-")
})

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
