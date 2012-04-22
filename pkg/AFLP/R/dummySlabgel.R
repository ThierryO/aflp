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

	if(!require(mvtnorm)){
		stop("This function requires the mvtnorm package")
	}
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
