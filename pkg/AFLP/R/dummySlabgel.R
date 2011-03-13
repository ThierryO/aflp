dummySlabgel <- function(nSpecimen = 50, nGroup = 2, nMarker = 50,
	nReplicate = 3,
	markerProb = c(Monomorph = 0.2, Group = 0.5),
	fixed = c(Intercept = 10, MarkerTrend = -0.008),
	maxGap = 10, betaShape = c(0.3, 0.8),
	SD = c(Specimen = 0.3, Replicate = 0.4, Marker = 0.6, Plate = 0.3, Noise = 1),
	transformation = c("log", "logit", "none")){

	transformation <- match.arg(transformation)
	dummy <- randomiseSlabgel(
		nSpecimen, 
		Group = sample(factor(LETTERS[seq_len(nGroup)]), nSpecimen, replace = TRUE), 
		nReplicate = nReplicate)
	SpecimenEffect <- rnorm(length(levels(replicates(dummy)$Specimen)), sd = SD["Specimen"])
	ReplicateEffect <- rnorm(length(levels(replicates(dummy)$Replicate)), sd = SD["Replicate"])
	PlateEffect <- rnorm(levels(replicates(dummy)$Plate), sd = SD["Plate"])
	Marker <- sample(800, nMarker, replace = FALSE)
	MarkerEffect <- rnorm(nMarker, sd = SD["Marker"])
	MarkerGap <- runif(nMarker, 0, maxGap)
	TypeMarker <- factor(sample(c("Monomorph", "Group", "Global"), nMarker, replace = TRUE, prob = c(markerProb["Monomorph"], markerProb["Group"], 1- sum(markerProb))))
	Fluor <- expand.grid(PC = factor("PC1"), Replicate = replicates(dummy)$Replicate, Marker = Marker, Fluorescence = NA, Normalised = NA, Score = NA)
	Fluor <- merge(Fluor, merge(replicates(dummy)[, c("Replicate", "Specimen", "Capilar", "Plate")], specimens(dummy)))
	Fluor$fMarker <- factor(Fluor$Marker)
	Fluor$TypeMarker <- TypeMarker[Fluor$fMarker]
	Fluor$Gap <- MarkerGap[Fluor$fMarker]
	Fluor <- ddply(Fluor, .(TypeMarker, Marker), function(x){
		if(x$TypeMarker[1] == "Monomorph"){
			data.frame(x, Truth = rbinom(1, size = 1, prob = 0.5))
		} else if(x$TypeMarker[1] == "Global"){
			data.frame(x , Truth = rbinom(nrow(x), size = 1, prob = rbeta(1, betaShape[1], betaShape[2])))
		} else{
			ddply(x, .(Group), function(y){
				data.frame(y , Truth = rbinom(nrow(y), size = 1, prob = rbeta(1, betaShape[1], betaShape[2])))
			})
		}
	})
	Fluor$Fluorescence <- with(Fluor, fixed["Intercept"] + SpecimenEffect[Specimen] + ReplicateEffect[Replicate] + PlateEffect[Plate] + MarkerEffect[fMarker] + fixed["MarkerTrend"] * Marker + Gap * (Truth -0.5) + rnorm(nrow(Fluor), sd = SD["Noise"]))
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
