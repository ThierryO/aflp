#'Simulate some data for a slab gel.
#'
#'Simulate some data for a slab gel.
#'
#'
#'@param nSpecimen The number of specimens. Defaults to 50.
#'@param nGroup Number of a priori groups in the dataset. Defaults to 2.
#'@param nMarker Number of markers in the dataset. Defaults to 50.
#'@param nReplicate The number of specimens that are replicated on each plate.
#'Defaults to 3.
#'@param markerProb A named vector of length 2 with the probability that a
#'marker is monomorph and the probability that a marker has a different
#'proportion of presence between groups.  Defaults to c(Monomorph = 0.2, Group
#'= 0.5).
#'@param fixed Overall mean fluorescence, overall difference in magnitude
#'between present and absent bands and trend in fluorescence along the Defaults
#'to c(Intercept = 9, Score = 1.7, MarkerTrend = -0.008).
#'@param betaShape Shapeparameters of a beta distribution used to generate the
#'probability that a marker is present. Defaults to c(0.3, 0.8).
#'@param VarCov A list of variance covariance matrices of the specimens,
#'replicates, markers, plates and noise. Defaults to list( Specimen =
#'matrix(c(0.080, -0.017, -0.017, 0.025), ncol = 2), Replicate =
#'matrix(c(0.116, -0.059, -0.059, 0.043), ncol = 2), Marker = matrix(c(0.581,
#'-0.236, -0.236, 0.326), ncol = 2), Plate = matrix(c(0.154, -0.035, -0.035,
#'0.027), ncol = 2), Noise = 0.292 ).
#'@param transformation Which transformation to use on the raw fluorescence
#'data. Valid choises are "log", "logit" and "none". Defaults to "log". "log"
#'implies the use of log(), hence no zero fluorescences are allowed. With
#'"logit", the raw fluorescence is first devided by the smallest power of 2,
#'which is still larger than the largest raw fluorescence. Then a logit
#'transformation is applied (\code{log(p/(1 - p))}). Defaults to "log".
#'@return An AFLP object with dummy fluoresence data. The fluorescence slot has
#'an extra binary variable "Truth".
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{randomiseSlabgel}}, \code{\link{AFLP}}
#'@keywords design
#'@examples
#'
#'	dummy <- dummySlabgel()
#'
#'@importFrom mvtnorm rmvnorm
#'@export
dummySlabgel <- function(nSpecimen = 50, nGroup = 2, nMarker = 50,
	nReplicate = 3,
	markerProb = c(Monomorph = 0.2, Group = 0.5),
	fixed = c(Intercept = 9, Score = 1.7, MarkerTrend = -0.007),
	betaShape = c(0.3, 0.8),
	VarCov = list(
		Specimen = matrix(c(0.080, -0.017, -0.017, 0.025), ncol = 2), 
		Replicate = matrix(c(0.116, -0.059, -0.059, 0.043), ncol = 2), 
		Marker = matrix(c(0.581, -0.236, -0.236, 0.326), ncol = 2), 
		Plate = matrix(c(0.154, -0.035, -0.035, 0.027), ncol = 2), 
		Noise = 0.292
	),
	transformation = c("log", "logit", "none")){

	transformation <- match.arg(transformation)
	dummy <- randomiseSlabgel(
		nSpecimen, 
		Group = sample(factor(LETTERS[seq_len(nGroup)]), nSpecimen, replace = TRUE), 
		nReplicates = nReplicate)
	Marker <- sample(800, nMarker, replace = FALSE)
	SpecimenEffect <- rmvnorm(length(levels(replicates(dummy)$Specimen)), sigma = VarCov[["Specimen"]])
	ReplicateEffect <- rmvnorm(length(levels(replicates(dummy)$Replicate)), sigma = VarCov[["Replicate"]])
	PlateEffect <- rmvnorm(length(levels(replicates(dummy)$Plate)), sigma = VarCov[["Plate"]])
	MarkerEffect <- rmvnorm(nMarker, sigma = VarCov[["Marker"]])
	TypeMarker <- factor(sample(c("Monomorph", "Group", "Global"), nMarker, replace = TRUE, prob = c(markerProb["Monomorph"], markerProb["Group"], 1- sum(markerProb))))
	Fluor <- expand.grid(PC = factor("PC1"), Replicate = replicates(dummy)$Replicate, Marker = Marker, Fluorescence = NA, Normalised = NA, Score = NA)
	Fluor <- merge(Fluor, merge(replicates(dummy)[, c("Replicate", "Specimen", "Capilar", "Plate")], specimens(dummy)))
	Fluor$fMarker <- factor(Fluor$Marker)
	Fluor$TypeMarker <- TypeMarker[Fluor$fMarker]
	Fluor <- ddply(Fluor, .(TypeMarker, Marker), function(x){
		if(x$TypeMarker[1] == "Monomorph"){
			data.frame(x, Truth = rbinom(1, size = 1, prob = 0.5))
		} else if(x$TypeMarker[1] == "Global"){
			merge(x, data.frame(Specimen = unique(x$Specimen), Truth = rbinom(length(unique(x$Specimen)), size = 1, prob = rbeta(1, betaShape[1], betaShape[2]))))
		} else{
			ddply(x, "Group", function(y){
				merge(y, data.frame(Specimen = unique(y$Specimen), Truth = rbinom(length(unique(y$Specimen)), size = 1, prob = rbeta(1, betaShape[1], betaShape[2]))))
			})
		}
	})
	Fluor$Gap <- with(Fluor, 
		fixed["Score"] + SpecimenEffect[Specimen, 2] + ReplicateEffect[Replicate, 2] + 
		PlateEffect[Plate, 2]  + MarkerEffect[fMarker, 2] 
		)
	Fluor$Fluorescence <- with(Fluor, 
		fixed["Intercept"] + fixed["MarkerTrend"] * Marker + Gap * Truth + 
		SpecimenEffect[Specimen, 1] + ReplicateEffect[Replicate, 1] + PlateEffect[Plate, 1] + 
		MarkerEffect[fMarker, 1] + rnorm(nrow(Fluor), sd = sqrt(VarCov[["Noise"]])))
	if(transformation == "log"){
		Fluor$Fluorescence <- exp(Fluor$Fluorescence)
	} else if(transformation == "logit"){
		Fluor$Fluorescence <- plogis(Fluor$Fluorescence) * 2 ^ 16
	}
	fluorescence(dummy) <- 
		Fluor[, 
			c("PC", "Plate", "Replicate", "Fluorescence", "Marker", "Normalised", "Score", "Truth", "Gap", "TypeMarker")
		]
	return(dummy)
}
