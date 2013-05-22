#'Extracts the specimen information from an object
#'
#'Extracts the specimen information from an object
#'
#'
#'@param x an AFLP or AFLP.outlier object
#'@return The specimen information of the object
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso
#'\code{\link{specimens-methods}},\code{\link{AFLP-class}},\code{\link{AFLP.outlier-class}}
#'@keywords attribute
#'@examples
#'
#'  data(Tilia)
#'	specimens(Tilia)
#'	specimens(outliers(Tilia))
#'@export
setGeneric("specimens", function(x) {
	standardGeneric("specimens")
})
#'Methods for extracting specimen information
#'
#'Extracts the information on specimen
#'
#'
#'@name specimens-methods
#'@aliases specimens-methods specimens,AFLP-method
#'specimens,AFLP.outlier-method
#'@docType methods
#'@section Methods: \describe{
#'
#'\item{x = "AFLP"}{return a vector with the specimen}
#'
#'\item{x = "AFLP.outlier"}{Return a dataframe with the specimen that are
#'marked as outliers} }
#'@keywords methods attribute
#'@export
setMethod("specimens", "AFLP.outlier", function(x) {
	x@Specimen
})
setMethod("specimens", "AFLP", function(x) {
	x@Specimens
})
