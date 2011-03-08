setGeneric("markers", function(x) {
	standardGeneric("markers")
})
setMethod("markers", "AFLP.outlier", function(x) {
	x@Marker
})
