setGeneric("replicates", function(x) {
	standardGeneric("replicates")
})
setMethod("replicates", "AFLP.outlier", function(x) {
	x@Replicate
})
setMethod("replicates", "AFLP", function(x) {
	x@Replicates
})

setGeneric("replicates<-", function(data, value){
	standardGeneric("replicates<-")
})
setMethod("replicates<-", signature(data = "AFLP"), function(data, value){
	data@Replicates <- value
	data
})
