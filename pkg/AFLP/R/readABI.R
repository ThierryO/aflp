#'Append fluorescence data from a ABI file to an AFLP object
#'
#'Append fluorescence data from a ABI file to an AFLP object.
#'
#'
#'@param filename The ABI file to be imported.
#'@param add.to The AFLP object were the fluorescence is appended to.
#'@return An AFLP object with the fluorescence data appended to it. Make sure
#'to use the \code{\link{defineBins}} function prior to normalise to data.
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{defineBins}}, \code{\link{as.AFLP}}
#'@keywords manip
#'@export
readABI <- function(filename, add.to){
	if(tail(strsplit(filename, "\\.")[[1]], 1) == "gz"){
		dataset <- read.delim(gzfile(filename), stringsAsFactors = FALSE)
	} else {
		dataset <- read.delim(filename, stringsAsFactors = FALSE)
	}
	fluorescence(add.to) <- 
		rbind(
			fluorescence(add.to),
			with(dataset, 
				subset(
					data.frame(
						PC = factor(Dye.Color), 
						Replicate = factor(Sample.File.Name), 
						Marker = Size, 
						Fluorescence = Height, 
						Normalised = NA, 
						Score = NA
					), 
					!is.na(Marker)
				)
			)
		)
	return(add.to)
}
