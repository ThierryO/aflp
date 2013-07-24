#'Extracts the quality control samples of an AFLP object
#'
#'@param data An AFLP object
#'@param which Which quality information to extract. Must be one of c("all",
#'"specimen", "replicate"). Defaults to "all".
#'@return In case when which == "all" a list with 2 data.frames containing the
#'quality control samples.  Otherwise a data.frame with the selected quality
#'control samples.
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{QC-method}}
#'@keywords attribute
#'@examples
#'
#'  data(Tilia)
#'	QC(Tilia)
#'
#'@exportMethod QC

setGeneric("QC", function(data, which = c("all", "specimen", "replicate")) {
	standardGeneric("QC")
})

#'Extracts the quality control samples of an AFLP object
#'
#'
#'@name QC-method
#'@aliases QC-method QC,AFLP-method
#'@docType methods
#'@section Methods: \describe{
#'
#'\item{x = "AFLP"}{Returns the QC information from an AFLP object}
#'}
#'@keywords methods attribute
#'@export

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

#'Adds or overwrites quality control samples
#'
#'Adds or overwrites quality control samples in an AFLP object.
#'
#'
#'@name QC<--methods
#'@aliases QC<--methods QC<-,AFLP-method
#'@docType methods
#'@section Methods: \describe{
#'
#'\item{data = "AFLP"}{\code{data} is updated with the new quality control
#'samples} }
#'@keywords methods
#'@exportMethod QC<-
setGeneric("QC<-", function(data, which, value){
	standardGeneric("QC<-")
})

#'Adds or overwrites quality control samples in an AFLP object.
#'
#'
#'@param data An AFLP object
#'@param value When which == "all", value must be a list with quality
#'information. Otherwise a data.frame with the corresponding quality
#'information.
#'@param which A character value matching c("all", "specimen", "replicate").
#'Defaults to "all".
#'@return \code{data} is updated with the new quality information
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{QC}}
#'@keywords manip
#'@examples
#'
#'  data(Tilia)
#'  tmp <- replicates(Tilia)
#'  QC(Tilia, which = "all") <- list(
#'    Specimen = data.frame(
#'      Specimen = tmp$Specimen[grep("QC", tmp$Specimen)], 
#'      Type = "method"
#'    ),
#'    Replicate = data.frame(
#'      Replicate = tmp$Replicate[grep("qc", tmp$Replicate)], 
#'      Type = "method"
#'    )
#'  )
#'  QC(Tilia, which = "specimen") <- data.frame(
#'    Specimen = tmp$Specimen[grep("QC", tmp$Specimen)], 
#'    Type = "method"
#'  )
#'  QC(Tilia, which = "replicate") <- data.frame(
#'    Replicate = tmp$Replicate[grep("qc", tmp$Replicate)], 
#'    Type = "method"
#'  )
#'

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
