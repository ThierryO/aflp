#'Checks if an object is of the AFLP class
#'
#'Checks if an object is of the AFLP class
#'
#'
#'@param x Some object
#'@return Logical. TRUE if the object if an AFLP object. FALSE otherwise.
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{AFLP-class}}, \code{\link{as.AFLP}}
#'@keywords attribute
#'@examples
#'
#'  n <- 100
#'  is.AFLP(n)
#'  data(Tilia)
#'  is.AFLP(Tilia)
#'@export
is.AFLP <- function(x){
	is(x, "AFLP")
}
