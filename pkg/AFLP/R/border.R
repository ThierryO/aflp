#'Extract the cut-levels from an AFLP object
#'
#'Extracts the cut-levels from an AFLP object
#'
#'
#'@name border-method
#'@aliases border-method border,AFLP-method
#'@docType methods
#'@section Methods: \describe{
#'
#'\item{data = "AFLP"}{Extracts the cut-levels from an AFLP object} }
#'@keywords methods attribute
#'@exportMethod border

setGeneric("border", function(data, pc, marker) {
	standardGeneric("border")
})

#'Extracts the cut-levels of an AFLP object
#'
#'Extracts the cut-levels of an AFLP object
#'
#'
#'@param data An AFLP object
#'@param pc The primer combination for which we want the cut-levels.
#'@param marker The marker for which we want the cut-levels.
#'@return A vector of cut-levels.
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{border-method}}
#'@keywords attribute
#'@examples
#'
#'  data(Tilia)
#'  BorderList <- border(Tilia)
#'
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




#'Adds or overwrites cut-levels
#'
#'Adds or overwrites cut-levels in an AFLP object.
#'
#'
#'@name border<--methods
#'@aliases border<--methods border<-,AFLP-method
#'@docType methods
#'@section Methods: \describe{
#'
#'\item{data = "AFLP"}{\code{data} is updated with the new border information}
#'}
#'@keywords methods
#'@exportMethod border<-
setGeneric("border<-", function(data, pc, marker, value){
	standardGeneric("border<-")
})

#'Adds or overwrites cut-levels
#'
#'Adds or overwrites cut-levels in an AFLP object.
#'
#'
#'@param data An AFLP object
#'@param value A single number of a numeric vector with the new cut-levels. Use
#'\code{Inf} to indicate a monomorph marker.
#'@param pc Label of the primer combination
#'@param marker Label of the marker
#'@return \code{data} is updated with the new border information
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{border}}
#'@keywords manip
#'@examples
#'
#'  data(Tilia)
#'  border(Tilia, pc = "PC1", marker = 116) <- Inf
#'
setMethod("border<-", signature(data = "AFLP"), function(data, pc, marker, value){
	if(!is.numeric(value)){
		stop("Only numerical values are accepted. Use 'Inf' to indicate no cut-level.")
	}
	PCmarker <- with(data@Borders, paste(PC, Marker))
	data@Borders <- rbind(data@Borders[!PCmarker %in% paste(pc, marker), ], data.frame(PC = pc, Marker = marker, Border = value))
	data
})
