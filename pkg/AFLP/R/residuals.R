setMethod("residuals", signature(object = "AFLP.outlier"), function(object, ...) {
	object@Residual
})
