#'Extract the replicates from an object
#'
#'Returns the replicates from an object
#'
#'
#'@param x An AFLP or AFLP.outlier object
#'@return The replicates of the object
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{replicates-method}}, \code{\link{AFLP-class}},
#'\code{\link{AFLP.outlier-class}}
#'@keywords attribute
#'@examples
#'
#'  data(Tilia)
#'	replicates(Tilia)
#'	replicates(outliers(Tilia))
#'
#'@export
setGeneric("replicates", function(x) {
	standardGeneric("replicates")
})

#'Extract replicates from an object
#'
#'Extract replicates from different kinds of objects
#'
#'
#'@name replicates-method
#'@aliases replicates-method replicates,AFLP-method
#'replicates,AFLP.outlier-method
#'@docType methods
#'@section Methods: \describe{
#'
#'\item{x = "AFLP"}{Returns the vector of replicates in an AFLP object}
#'
#'\item{x = "AFLP.outlier"}{Returns a data frame with the replicates marked as
#'outliers} }
#'@keywords methods attribute
#'@export
setMethod("replicates", "AFLP.outlier", function(x) {
	x@Replicate
})
setMethod("replicates", "AFLP", function(x) {
	x@Replicates
})

#'Adds or overwrites replicates
#'
#'Adds or overwrites replicates in an AFLP object.
#'
#'
#'@name replicates<--methods
#'@aliases replicates<--methods replicates<-,AFLP-method
#'@docType methods
#'@section Methods: \describe{
#'
#'\item{data = "AFLP"}{\code{data} is updated with the new replicates} }
#'@keywords methods
#'@export
setGeneric("replicates<-", function(data, value){
	standardGeneric("replicates<-")
})

#'Adds or overwrites the replicates
#'
#'Adds or overwrites the replicates in an AFLP object.
#'
#'
#'@param data An AFLP object
#'@param value A data.frame with the replicate information
#'@return \code{data} is updated with the new replicate information
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{replicate}}
#'@keywords manip
#'@examples
#'
#'  data(Tilia)
#'  tmp <- replicates(Tilia)
#'  tmp$Plate[1] <- "8"
#'  replicates(Tilia) <- tmp
#'
#'@export
setMethod("replicates<-", signature(data = "AFLP"), function(data, value){
	data@Replicates <- value
	data
})
