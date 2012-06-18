setMethod("princomp", signature(x = "AFLP"), function(x, method = c("normalised", "raw", "Jaccard"), axes = c(1, 2), screeplot = FALSE,...){
	method <- match.arg(method)
	if(all(is.na(fluorescence(x)$Score))){
		stop("AFLP data must be classified first")
	}
	
	if(method == "Jaccard"){
		rawData <- cast(Replicate ~ PC + Marker, data = fluorescence(x), value = "Score")
		rownames(rawData) <- rawData$Replicate
		rawData <- rawData[, -1]
		rawData[rawData > 1] <- 1
		distmat <- vegdist(rawData, method = "jaccard", na.rm = TRUE)
		Deleted <- NULL
		while(!all(rowSums(is.na(as.matrix(distmat))) == 0)){
			whichNA <- which(rowSums(is.na(as.matrix(distmat))) > 0)
			toDelete <- which.max(rowMeans(is.na(rawData[whichNA, ])))
			Deleted <- c(Deleted, rownames(rawData)[whichNA[toDelete]])
			message(rownames(rawData)[whichNA[toDelete]], " removed from analysis due to a large number of missing values")
			rawData <- rawData[-whichNA[toDelete], ]
			distmat <- vegdist(rawData, method = "jaccard", na.rm = TRUE)
		}
		PCA <- princomp(distmat)
	} else if(method == "raw"){
		rawData <- cast(Replicate ~ PC + Marker, data = fluorescence(x), value = "Fluorescence")
		rownames(rawData) <- rawData$Replicate
		rawData <- rawData[, -1]
		PCA <- princomp(dist(rawData), cor = TRUE)
	} else {
		rawData <- cast(Replicate ~ PC + Marker, data = fluorescence(x), value = "Normalised")
		rownames(rawData) <- rawData$Replicate
		rawData <- rawData[, -1]
		PCA <- princomp(dist(rawData), cor = TRUE)
	}
	rownames(PCA$scores) <- rownames(rawData)
	PCO <- data.frame(PCA$scores)
	PCO$Replicate <- rownames(rawData)
	PCO <- merge(PCO, replicates(x)[, c("Specimen", "Replicate")])
	PCO <- merge(PCO, specimens(x))
	if(screeplot){
		screeplot(PCA)
	} else {
		axes <- paste("Comp", axes, sep = ".")
		if(length(unique(PCO$Group)) > 1){
			print(ggplot(PCO, aes_string(x = axes[1], y = axes[2])) + geom_vline(xintercept = 0, linetype = 2) + geom_hline(yintercept = 0, linetype = 2) + geom_point() + geom_density2d() + coord_equal() + facet_wrap(~Group))
		} else {
			print(ggplot(PCO, aes_string(x = axes[1], y = axes[2])) + geom_point() + geom_density2d() + coord_equal())
		}
	}
	invisible(PCO)
})
