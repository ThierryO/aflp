setGeneric("outliers", function(x){
	standardGeneric("outliers")
})
setMethod("outliers", "AFLP", function(x) {
	x@outliers
})
