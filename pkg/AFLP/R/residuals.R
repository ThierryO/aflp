#setMethod("residuals", signature(object = "AFLP.outlier"), function(object, ...){
#	object@Residual
#})
#residuals.AFLP.outlier <- function(object, ...){
#	object@Residual
#}

#'Extracts the residual outliers
#'
#'Extract the residual outliers from an AFLP.outlier object
#'
#'
#'@name residuals-methods
#'@aliases residuals-methods residuals,AFLP.outlier-method
#'@docType methods
#'@section Methods: \describe{ \item{object = "AFLP.outlier"}{returns a
#'dataframe with the residual outliers} }
#'@keywords methods attribute
#'@export

setMethod("residuals", signature(object = "AFLP.outlier"), 
	function(object, ...){
		object@Residual
	}
)

#'Extracts the residual outliers
#'
#'Extract the residual outliers from an AFLP.outlier object
#'
#'
#'@name resid-methods
#'@aliases resid-methods resid,AFLP.outlier-method
#'@docType methods
#'@section Methods: \describe{ \item{object = "AFLP.outlier"}{returns a
#'dataframe with the residual outliers} }
#'@keywords methods attribute
#'@export

setMethod("resid", signature(object = "AFLP.outlier"), 
	function(object, ...){
		object@Residual
	}
)
