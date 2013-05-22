#'Extracts outliers
#'
#'Extracts outliers
#'
#'
#'@name outliers-methods
#'@aliases outliers-methods outliers,AFLP-method
#'@docType methods
#'@section Methods: \describe{
#'
#'\item{x = "AFLP"}{Extracts outliers from an AFLP object} }
#'@keywords methods attribute
#'@export

setGeneric("outliers", function(x){
	standardGeneric("outliers")
})

#'Extract outlier information from an AFLP object
#'
#'Extract outlier information from an AFLP object
#'
#'
#'@param x An AFLP object
#'@return An object of the AFLP.outlier class with the outliers from the AFLP
#'object
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{AFLP-class}}, \code{\link{AFLP.outlier-class}}
#'@keywords attribute
#'@examples
#'
#'  data(Tilia)
#'	outliers(Tilia)
#'
#'@export
setMethod("outliers", "AFLP", function(x) {
	x@outliers
})
