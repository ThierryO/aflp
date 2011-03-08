setClass("AFLP.outlier",
	representation = representation(Replicate = "data.frame", Specimen = "data.frame",
		Marker = "data.frame", Residual = "data.frame"),
	prototype = prototype(Replicate = data.frame(PC = character(), Replicate = character(), Observed = numeric()),
		Specimen = data.frame(PC = character(), Specimen = character(), Observed = numeric()),
		Marker = data.frame(PC = character(), Marker = numeric(), Observed = numeric()),
		Residual = data.frame(PC = character(), Replicate = character(), Marker = numeric(), Observed = numeric()))
)

AFLP.outlier <- function(
		Replicate = data.frame(PC = character(), Replicate = character(), Observed = numeric()), 
		Specimen = data.frame(PC = character(), Specimen = character(), Observed = numeric()), 
		Marker = data.frame(PC = character(), Marker = numeric(), Observed = numeric()), 
		Residual = data.frame(PC = character(), Replicate = character(), Marker = numeric(), Observed = numeric())){
	return(new("AFLP.outlier", Replicate = Replicate, Specimen = Specimen, Marker = Marker, Residual = Residual))
}
