#setMethod("residuals", signature(object = "AFLP.outlier"), function(object, ...){
#	object@Residual
#})
#residuals.AFLP.outlier <- function(object, ...){
#	object@Residual
#}
setMethod("residuals", signature(object = "AFLP.outlier"), 
	function(object, ...){
		object@Residual
	}
)

setMethod("resid", signature(object = "AFLP.outlier"), 
	function(object, ...){
		object@Residual
	}
)
