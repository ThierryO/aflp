setGeneric("specimens", function(x) {
	standardGeneric("specimens")
})
setMethod("specimens", "AFLP.outlier", function(x) {
	x@Specimen
})
setMethod("specimens", "AFLP", function(x) {
	x@Specimens
})
