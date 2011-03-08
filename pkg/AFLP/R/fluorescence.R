setGeneric("fluorescence", function(x) {
	standardGeneric("fluorescence")
})
setMethod("fluorescence", "AFLP", function(x) {
	x@Fluorescence
})

setGeneric("fluorescence<-", function(data, value){
	standardGeneric("fluorescence<-")
})
setMethod("fluorescence<-", signature(data = "AFLP", value = "data.frame"), function(data, value){
	data@Fluorescence <- value
	data
})
