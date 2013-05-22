#'Convert an object to a AFLP object
#'
#'This function allows to convert a list of specimen or a set of positions to
#'an AFLP object. This is a good starting point for an analysis.
#'
#'
#'@param x A data.frame or a matrix containing at least the columns Plate,
#'Capilar, Lane, Replicate and Specimen. The column Group is optional.
#'@param \dots more arguments passed to the function.
#'@return An AFLP object
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@keywords manip
#'@examples
#'
#'  data(TiliaDesign)
#'  Tilia <- as.AFLP(TiliaDesign)
#'
#'@export
as.AFLP <- function(x, ...){
	if(is.AFLP(x)){
		return(x)
	} else if(is.data.frame(x) || is.matrix(x)){
		if(all(c("Plate", "Capilar", "Lane", "Replicate", "Specimen") %in% colnames(x))){
			if(!"Group" %in% colnames(x)){
				x$Group <- factor(NA)
			} else {
				if(class(x$Group) != "factor"){
					x$Group <- factor(x$Group)
				}
			}
			if(class(x$Specimen) != "factor"){
				x$Specimen <- factor(x$Specimen)
			}
			if(class(x$Replicate) != "factor"){
				x$Replicate <- factor(x$Replicate)
			}
			if(class(x$Plate) != "factor"){
				x$Plate <- factor(x$Plate)
			}
			if(class(x$Capilar) != "factor"){
				x$Capilar <- factor(x$Capilar)
			}
			if(class(x$Lane) != "factor"){
				x$Lane <- factor(x$Lane)
			}
			message("Using matrix or dataframe as positions.")
			new("AFLP", 
				Specimens = unique(x[, c("Specimen", "Group")]),
				Replicates = unique(x[, c("Replicate", "Specimen", "Plate", "Capilar", "Lane")]),
				QC = list(Specimen = character(), Replicate = character), 
				Fluorescence = data.frame(PC = factor(), Replicate = factor(), Fluorescence = numeric(), Marker = numeric(), Normalised = numeric() , Score = factor()),
				model = log(Fluorescence) ~ 1, outliers = AFLP.outlier(),
				Borders = data.frame(PC = factor(), Marker = numeric(), Border = numeric()),
				Quality = list(
					Marker = data.frame(
						PC = factor(), Marker = numeric(), Score = numeric(), 
						Errors = integer(), MaxErrors = integer(), nBin = integer()
					), 
					Specimen = data.frame(
						PC = factor(), Specimen = factor(), Score = numeric(), 
						Errors = integer(), MaxErrors = integer(), nBin = integer(), 
						MaxErrorsAll = integer(), nBinAll = integer()
					),
					Overall = data.frame(
						PC = factor(), Score = numeric(), Errors = integer(), 
						MaxErrors = integer(), nBin = integer(), MaxErrorsAll = integer(), 
						nBinAll = integer()
					)
				)
			)
		} else {
			stop("The dataframe or matrix requires five columns with names: Plate, Capilar, Lane, Replicate and Specimen")
		}
	} else {
		stop("x must be either a matrix or dataframe with the positions of the replicates.")
	}
}
