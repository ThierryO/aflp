setMethod("hclust", signature(d = "AFLP"), function(d, method ="complete", members = NULL){
	if(all(is.na(fluorescence(d)$Score))){
		stop("AFLP data must be classified first")
	}
	rawData <- cast(Replicate ~ PC + Marker, data = fluorescence(d), value = "Score")
	rownames(rawData) <- rawData$Replicate
	rawData <- rawData[, -1]
	rawData[rawData > 1] <- 1
	Jaccard <- vegdist(rawData, method = "jaccard", na.rm = TRUE)
	Deleted <- NULL
	while(!all(rowSums(is.na(as.matrix(Jaccard))) == 0)){
		whichNA <- which(rowSums(is.na(as.matrix(Jaccard))) > 0)
		toDelete <- which.max(rowMeans(is.na(rawData[whichNA, ])))
		Deleted <- c(Deleted, rownames(rawData)[whichNA[toDelete]])
		message(rownames(rawData)[whichNA[toDelete]], " removed from analysis due to a large number of missing values")
		rawData <- rawData[-whichNA[toDelete], ]
		Jaccard <- vegdist(rawData, method = "jaccard", na.rm = TRUE)
	}
	rawData <- cast(Replicate ~ PC + Marker, data = fluorescence(d), value = "Normalised")
	rawData <- rawData[!rawData$Replicate %in% Deleted, ]
	rownames(rawData) <- rawData$Replicate
	rawData <- rawData[, -1]
	Grey <- dist(rawData)
	HC <- hclust(Jaccard, method = method, members = NULL)
	HCG <- hclust(Grey, method = method, members = NULL)
	Samples <- merge(replicates(d)[, c("Replicate", "Specimen")], specimens(d))
	rownames(Samples) <- Samples$Replicate
	X11()
	plot(HC, labels = rownames(rawData))
	X11()
	plot(HC, labels = Samples[rownames(rawData), "Specimen"])
	if(!all(is.na(Samples$Group))){
		X11()
		plot(HC, labels = Samples[rownames(rawData), "Group"])
	}
	X11()
	plot(HCG, labels = rownames(rawData))
	X11()
	plot(HCG, labels = Samples[rownames(rawData), "Specimen"])
	if(!all(is.na(Samples$Group))){
		X11()
		plot(HCG, labels = Samples[rownames(rawData), "Group"])
	}
	invisible(list(Jaccard = HC, Grey = HCG, Deleted = Deleted))
})
