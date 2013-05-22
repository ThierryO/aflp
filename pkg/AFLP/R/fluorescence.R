#'Extracting fluorescence from an AFLP object
#'
#'Extracting fluorescence from an AFLP object
#'
#'
#'@name fluorescence-methods
#'@aliases fluorescence-methods fluorescence,AFLP-method
#'@docType methods
#'@section Methods: \describe{
#'
#'\item{x = "AFLP"}{Extracting fluorescence from \code{x}} }
#'@keywords methods attribute
#'@export
setGeneric("fluorescence", function(x) {
	standardGeneric("fluorescence")
})

#'Extract the fluorescence (raw, normalised and scored) information from an
#'AFLP object
#'
#'Extract the fluorescence (raw, normalised and scored) information from an
#'AFLP object
#'
#'
#'@param x An AFLP object
#'@return The \code{Fluorescence} slot of \code{x}.
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{AFLP-class}}
#'@keywords attribute
#'@examples
#'
#'  data(Tilia)
#'  fluorescence(Tilia)
#'
setMethod("fluorescence", "AFLP", function(x) {
	x@Fluorescence
})


#'Adds or overwrites fluorescence data
#'
#'Adds or overwrites fluorescence data in an AFLP object.
#'
#'
#'@name fluorescence<--methods
#'@aliases fluorescence<--methods fluorescence<-,AFLP,data.frame-method
#'@docType methods
#'@section Methods: \describe{
#'
#'\item{data = "AFLP", value = "data.frame"}{\code{data} is updated with the
#'new fluorescence data} }
#'@keywords methods
#'@export
setGeneric("fluorescence<-", function(data, value){
	standardGeneric("fluorescence<-")
})

#'Adds or overwrites the fluorescence data
#'
#'Adds or overwrites the fluorescence in an AFLP object.
#'
#'
#'@param data An AFLP object
#'@param value The new fluorescence data
#'@return \code{data} is updated with the new fluorescence data
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{fluorescence}}
#'@keywords manip
#'@examples
#'
#'  data(Tilia)
#'  Fl <- fluorescence(Tilia)
#'  Fl$Fluorescence[10] <- NA
#'  fluorescence(Tilia) <- Fl
#'
#'@export
setMethod("fluorescence<-", signature(data = "AFLP", value = "data.frame"), function(data, value){
	data@Fluorescence <- value
	data
})
