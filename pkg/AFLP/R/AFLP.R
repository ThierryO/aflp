setClass("AFLP", 
	representation = representation(Specimens = "data.frame", 
		Replicates = "data.frame", QC = "list", Fluorescence = "data.frame", 
		model = "formula", outliers = "AFLP.outlier", Borders = "data.frame",
		Quality = "list"),
	prototype = prototype(Specimens = data.frame(Specimen = factor(), Group = factor()), 
		Replicates = data.frame(Replicate = factor(), Specimen = factor(), Plate = factor(), 
			Capilar = factor(), Lane = factor()), 
		QC = list(Specimen = data.frame(Specimen = factor(), Type = factor()), Replicate = data.frame(Replicate = factor(), Type = factor())), 
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
)

AFLP <- function(Specimens, Replicates, QC, Fluorescence, model, outliers, Borders, Quality){
	return(new("AFLP", Specimens = Specimens, Replicates = Replicates, QC = QC, 
		Fluorescence = Fluorescence, model = model, outliers = outliers, 
		Borders = Borders, Quality = Quality))
}
