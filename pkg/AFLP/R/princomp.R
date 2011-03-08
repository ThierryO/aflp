setMethod("princomp", signature(x = "AFLP"), function(x, ...){
	if(!require(vegan)){
		stop("The vegan package is required for the PCO analysis")
	}
	if(!require(ggplot2)){
		stop("The ggplot2 package is required for the PCO analysis")
	}
	if(all(is.na(fluorescence(x)$Score))){
		stop("AFLP data must be classified first")
	}
	rawData <- cast(Replicate ~ PC + Marker, data = fluorescence(x), value = "Score")
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
	PCA <- princomp(Jaccard)
	rownames(PCA$scores) <- rownames(rawData)
	
	rawData <- cast(Replicate ~ PC + Marker, data = fluorescence(x), value = "Normalised")
	rawData <- rawData[!rawData$Replicate %in% Deleted, ]
	rownames(rawData) <- rawData$Replicate
	rawData <- rawData[, -1]
	PCAG <- princomp(dist(rawData), cor = TRUE)
	rownames(PCAG$scores) <- rownames(rawData)
	
	PCO <- data.frame(PCA$scores)
	PCO$Replicate <- rownames(rawData)
	PCO <- merge(PCO, replicates(x)[, c("Specimen", "Replicate")])
	PCO <- merge(PCO, specimens(x))
	X11()
	screeplot(PCA)
	if(length(unique(PCO$Group)) > 1){
		X11()
		print(ggplot(PCO, aes(x = Comp.1, y = Comp.2)) + geom_point(aes(colour = Group)) + geom_density2d() + coord_equal() + opts(title = "PCO on scores"))
		X11()
		if(min(table(PCO$Group)) > 1){
			print(ggplot(PCO, aes(x = Comp.1, y = Comp.2)) + geom_point() + geom_density2d()+ coord_equal() + opts(title = "PCO on scores") + facet_wrap(~Group) + geom_vline(xintercept = 0, linetype = 2) + geom_hline(yintercept = 0, linetype = 2))
		} else {
			print(ggplot(PCO, aes(x = Comp.1, y = Comp.2)) + geom_point() + coord_equal() + opts(title = "PCO on scores") + facet_wrap(~Group) + geom_vline(xintercept = 0, linetype = 2) + geom_hline(yintercept = 0, linetype = 2))
		}
		X11()
		print(ggplot(PCO, aes(x = Comp.1, y = Comp.3)) + geom_point(aes(colour = Group)) + geom_density2d() + coord_equal() + opts(title = "PCO on scores"))
		X11()
		print(ggplot(PCO, aes(x = Comp.2, y = Comp.3)) + geom_point(aes(colour = Group)) + geom_density2d() + coord_equal() + opts(title = "PCO on scores"))
		X11()
		print(ggplot(PCO, aes(x = Comp.1, y = Comp.4)) + geom_point(aes(colour = Group)) + geom_density2d() + coord_equal() + opts(title = "PCO on scores"))
		X11()
		print(ggplot(PCO, aes(x = Comp.2, y = Comp.4)) + geom_point(aes(colour = Group)) + geom_density2d() + coord_equal() + opts(title = "PCO on scores"))
		X11()
		print(ggplot(PCO, aes(x = Comp.3, y = Comp.4)) + geom_point(aes(colour = Group)) + geom_density2d() + coord_equal() + opts(title = "PCO on scores"))
	} else{
		X11()
		print(ggplot(PCO, aes(x = Comp.1, y = Comp.2)) + stat_density2d(aes(fill = ..density..), geom="tile", contour = FALSE) + geom_point() + coord_equal() + opts(title = "PCO on scores"))
		X11()
		print(ggplot(PCO, aes(x = Comp.1, y = Comp.3)) + stat_density2d(aes(fill = ..density..), geom="tile", contour = FALSE) + geom_point() + coord_equal() + opts(title = "PCO on scores"))
		X11()
		print(ggplot(PCO, aes(x = Comp.2, y = Comp.3)) + stat_density2d(aes(fill = ..density..), geom="tile", contour = FALSE) + geom_point() + coord_equal() + opts(title = "PCO on scores"))
		X11()
		print(ggplot(PCO, aes(x = Comp.1, y = Comp.4)) + stat_density2d(aes(fill = ..density..), geom="tile", contour = FALSE) + geom_point() + coord_equal() + opts(title = "PCO on scores"))
		X11()
		print(ggplot(PCO, aes(x = Comp.2, y = Comp.4)) + stat_density2d(aes(fill = ..density..), geom="tile", contour = FALSE) + geom_point() + coord_equal() + opts(title = "PCO on scores"))
		X11()
		print(ggplot(PCO, aes(x = Comp.3, y = Comp.4)) + stat_density2d(aes(fill = ..density..), geom="tile", contour = FALSE) + geom_point() + coord_equal() + opts(title = "PCO on scores"))
	}
	
	PCO <- data.frame(PCAG$scores)
	PCO$Replicate <- rownames(rawData)
	PCO <- merge(PCO, replicates(x)[, c("Specimen", "Replicate")])
	PCO <- merge(PCO, specimens(x))
	X11()
	screeplot(PCAG)
	if(length(unique(PCO$Group)) > 1){
		X11()
		print(ggplot(PCO, aes(x = Comp.1, y = Comp.2)) + geom_point(aes(colour = Group)) +  geom_density2d() + coord_equal() + opts(title = "PCA on normalised fluorescence"))
		X11()
		if(min(table(PCO$Group)) > 1){
			print(ggplot(PCO, aes(x = Comp.1, y = Comp.2)) + geom_point() + geom_density2d() + coord_equal() + opts(title = "PCA on normalised fluorescence") + facet_wrap(~Group) + geom_vline(xintercept = 0, linetype = 2) + geom_hline(yintercept = 0, linetype = 2))
		} else {
			print(ggplot(PCO, aes(x = Comp.1, y = Comp.2)) + geom_point() + coord_equal() + opts(title = "PCA on normalised fluorescence") + facet_wrap(~Group) + geom_vline(xintercept = 0, linetype = 2) + geom_hline(yintercept = 0, linetype = 2))
		}
		X11()
		print(ggplot(PCO, aes(x = Comp.1, y = Comp.3)) + geom_point(aes(colour = Group)) +  geom_density2d() + coord_equal() + opts(title = "PCA on normalised fluorescence"))
		X11()
		print(ggplot(PCO, aes(x = Comp.2, y = Comp.3)) + geom_point(aes(colour = Group)) +  geom_density2d() + coord_equal() + opts(title = "PCA on normalised fluorescence"))
		X11()
		print(ggplot(PCO, aes(x = Comp.1, y = Comp.4)) + geom_point(aes(colour = Group)) +  geom_density2d() + coord_equal() + opts(title = "PCA on normalised fluorescence"))
		X11()
		print(ggplot(PCO, aes(x = Comp.2, y = Comp.4)) + geom_point(aes(colour = Group)) +  geom_density2d() + coord_equal() + opts(title = "PCA on normalised fluorescence"))
		X11()
		print(ggplot(PCO, aes(x = Comp.3, y = Comp.4)) + geom_point(aes(colour = Group)) +  geom_density2d() + coord_equal() + opts(title = "PCA on normalised fluorescence"))
	} else {
		X11()
		print(ggplot(PCO, aes(x = Comp.1, y = Comp.2)) + stat_density2d(aes(fill = ..density..), geom="tile", contour = FALSE) + geom_point() + coord_equal() + opts(title = "PCO on normalised fluorescence"))
		X11()
		print(ggplot(PCO, aes(x = Comp.1, y = Comp.3)) + stat_density2d(aes(fill = ..density..), geom="tile", contour = FALSE) + geom_point() + coord_equal() + opts(title = "PCO on normalised fluorescence"))
		X11()
		print(ggplot(PCO, aes(x = Comp.2, y = Comp.3)) + stat_density2d(aes(fill = ..density..), geom="tile", contour = FALSE) + geom_point() + coord_equal() + opts(title = "PCO on normalised fluorescence"))
		X11()
		print(ggplot(PCO, aes(x = Comp.1, y = Comp.4)) + stat_density2d(aes(fill = ..density..), geom="tile", contour = FALSE) + geom_point() + coord_equal() + opts(title = "PCO on normalised fluorescence"))
		X11()
		print(ggplot(PCO, aes(x = Comp.2, y = Comp.4)) + stat_density2d(aes(fill = ..density..), geom="tile", contour = FALSE) + geom_point() + coord_equal() + opts(title = "PCO on normalised fluorescence"))
		X11()
		print(ggplot(PCO, aes(x = Comp.3, y = Comp.4)) + stat_density2d(aes(fill = ..density..), geom="tile", contour = FALSE) + geom_point() + coord_equal() + opts(title = "PCO on normalised fluorescence"))
	}
	invisible(list(Jaccard = PCA, Grey = PCAG, Deleted = Deleted))
})
