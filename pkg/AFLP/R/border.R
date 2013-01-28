setGeneric("border", function(data, pc, marker) {
	standardGeneric("border")
})
setMethod("border", "AFLP", function(data, pc, marker) {
  #fooling R CMD check
  PC <- NULL
  #fooling R CMD check
	if(missing(pc)){
		data@Borders
	} else if(missing(marker)){
		subset(data@Borders, PC %in% pc)
	} else {
		PCmarker <- with(data@Borders, paste(PC, Marker))
		data@Borders$Border[PCmarker %in% paste(pc, marker)]
	}
	
})

setGeneric("border<-", function(data, pc, marker, value){
	standardGeneric("border<-")
})
setMethod("border<-", signature(data = "AFLP"), function(data, pc, marker, value){
	if(!is.numeric(value)){
		stop("Only numerical values are accepted. Use 'Inf' to indicate no cut-level.")
	}
	PCmarker <- with(data@Borders, paste(PC, Marker))
	data@Borders <- rbind(data@Borders[!PCmarker %in% paste(pc, marker), ], data.frame(PC = pc, Marker = marker, Border = value))
	data
})
