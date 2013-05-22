#'Class "AFLP"
#'
#'This class is design to hold all relevant information on an AFLP project.
#'
#'
#'@name AFLP-class
#'@docType class
#'@section Objects from the Class: Objects can be created from scratch by
#'\code{\link{AFLP}}. But we recommend to start with a set of specimens that
#'are randomised. This ensures that a sufficient number of specimens are
#'replicated. If the specimens are already distributed among the plates use
#'\code{\link{as.AFLP}} to convert the positions to an AFLP object.
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{AFLP}}, \code{\link{as.AFLP}}, \code{\link{is.AFLP}},
#'\code{\link{border}}, \code{\link{fluorescence}}, \code{\link{hclust}},
#'\code{\link{outliers}}, \code{\link{quality}}, \code{\link{QC}},
#'\code{\link{princomp}}, \code{\link{replicates}}, \code{\link{specimens}}
#'@keywords classes
#'@examples
#'
#'  data(Tilia)
#'
#'@exportClass AFLP

setClass("AFLP", 
	representation = representation(Specimens = "data.frame", 
		Replicates = "data.frame", QC = "list", Fluorescence = "data.frame", 
		model = "formula", outliers = "AFLP.outlier", Borders = "data.frame",
		Quality = "list"),
	prototype = prototype(Specimens = data.frame(Specimen = factor(), Group = factor()), 
		Replicates = data.frame(Replicate = factor(), Specimen = factor(), Plate = factor(), 
			Capilar = factor(), Lane = factor()), 
		QC = list(
			Specimen = data.frame(
				Specimen = factor(), Type = factor()), 
			Replicate = data.frame(Replicate = factor(), 
				Type = factor())
		), 
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
			Replicate = data.frame(
				PC = factor(), Specimen = factor(), ReplicateA = factor(),
				ReplicateB = factor(), Score = numeric(), Errors = integer(), MaxErrors = integer()
			),
			Plate = data.frame(
				PC = factor(), PlateA = factor(), PlateB = factor(),
				Score = numeric(), Errors = integer(), MaxErrors = integer()
			),
			Primercombination = data.frame(
				PC = factor(), Score = numeric(), Errors = integer(), 
				MaxErrors = integer(), nBin = integer(), MaxErrorsAll = integer(), 
				nBinAll = integer()
			),
			Global = data.frame(
				Type = factor(), Score = numeric(), Errors = integer(), 
				MaxErrors = integer()
			)
		)
	)
)

#' Create an object of the class AFLP
#' 
#' @param Specimens A data.frame with specimens
#' @param Replicates A data.frame with replicates
#' @param QC A list with QC samples
#' @param Fluorescence A data.Frame with fluorescence data
#' @param model The formula of the model for normalisation
#' @param outliers AFLP outliers
#' @param Borders The borders between present and absent markers
#' @param Quality Quality data
#' @export
AFLP <- function(Specimens, Replicates, QC, Fluorescence, model, outliers, Borders, Quality){
	return(new("AFLP", Specimens = Specimens, Replicates = Replicates, QC = QC, 
		Fluorescence = Fluorescence, model = model, outliers = outliers, 
		Borders = Borders, Quality = Quality))
}
