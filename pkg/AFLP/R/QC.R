setGeneric("QC", function(data, which = c("all", "specimen", "replicate")) {
	standardGeneric("QC")
})
setMethod("QC", signature(data = "AFLP"), function(data, which = c("all", "specimen", "replicate")) {
	which <- match.arg(which)
	if(which == "all"){
		return(data@QC)
	} else if(which == "specimen"){
		return(data@QC[["Specimen"]])
	} else {
		return(data@QC[["Replicate"]])
	}
})

setGeneric("QC<-", function(data, which, value){
	standardGeneric("QC<-")
})

setMethod("QC<-", signature(data = "AFLP"), function(data, which = c("all", "specimen", "replicate"), value){
	which <- match.arg(which)
	if(which == "all"){
		data@QC <- value
	} else if(which == "specimen"){
		data@QC[["Specimen"]] <- value
	} else {
		data@QC[["Replicate"]] <- value
	}
	return(data)
})
