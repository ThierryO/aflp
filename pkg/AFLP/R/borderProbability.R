#'Internal functions of the AFLP package.
#'
#'Not intended for external use.
#'
#'
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{AFLP-class}}
#'@keywords internal
#'@export
borderProbability <- function(Fluor, Borders, Repeated){
	ProbPresent <- sapply(Borders, function(x){
		mean(Fluor$Normalised >= x)
	})
	Testset <- merge(Repeated, Fluor)[, c("Specimen", "Normalised")]
	Multis <- table(Testset$Specimen)
	Testset <- Testset[Testset$Specimen %in% names(Multis)[Multis > 1], ]
	RandomProbability <- ddply(Testset, "Specimen", function(z){
		Present <- sapply(Borders, function(x){sum(z$Normalised >= x)})
		Delta <- apply(cbind(Present, nrow(z) - Present), 1, min)
		ProbError <- data.frame(Errors = c(0:floor(nrow(z) / 2), floor((nrow(z) - 1) / 2):0),
			sapply(ProbPresent, function(prob){
				dbinom(0:nrow(z), nrow(z), prob = prob)
			})
		)
		sapply(seq_along(Delta), function(i){
			sum(ProbError[ProbError$Errors <= Delta[i], i + 1])
		})
	})[, -1, drop = FALSE]
	as.vector(apply(RandomProbability, 2, prod))
}
