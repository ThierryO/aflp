#'Extract the marker information from an object
#'
#'Extract the marker information from an object
#'
#'
#'@name markers-method
#'@aliases markers-method markers,AFLP.outlier-method
#'@docType methods
#'@section Methods: \describe{
#'
#'\item{x = "AFLP.outlier"}{Extract the marker information from an AFLP.outlier
#'object} }
#'@keywords methods attribute
#'@export
setGeneric("markers", function(x) {
	standardGeneric("markers")
})

#'Extract the marker information from an object
#'
#'Extract the marker information from an object
#'
#'
#'@param x An AFLP object or an AFLP.outlier object
#'@return the marker information from an object
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@keywords attributes
#'@examples
#'
#'  data(Tilia)
#'  mTilia <- markers(outliers(Tilia))
#'
#'@exportMethod markers
setMethod("markers", "AFLP.outlier", function(x) {
	x@Marker
})
