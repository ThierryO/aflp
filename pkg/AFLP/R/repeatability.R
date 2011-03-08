repeatability <- function(data, output = c("screen", "tex", "none"), bootstrap = FALSE, minMarker = NULL, path = NULL, device = "pdf"){
	output <- match.arg(output)
	if(output != "none"){
		if(!require(ggplot2)){
			stop("The ggplot2 package is required to create the graphics.")
		}
		if(output == "tex"){
			if(!require(xtable)){
				stop("The xtable package is required to create LaTeX output.")
			}
		}
	}
	Repeated <- replicates(data)[, c("Specimen", "Replicate", "Capilar", "Lane")]
	if(nrow(replicates(outliers(data))) > 0){
		Repeated <- subset(merge(Repeated, cbind(replicates(outliers(data)), remove = TRUE), all.x = TRUE), is.na(remove))[, c("Specimen", "Replicate", "Lane")]
	}
	if(nrow(specimens(outliers(data))) > 0){
		Repeated <- subset(merge(Repeated, cbind(specimens(outliers(data)), remove = TRUE), all.x = TRUE), is.na(remove))[, c("Specimen", "Replicate", "Lane")]
	}
	Repeated2 <- cast(Specimen ~ ., data = Repeated, value = "Lane", fun = length)
	Repeated <- subset(Repeated, Specimen %in% Repeated2$Specimen[Repeated2[, 2] > 1])
	rawData <- fluorescence(data)
	if(nrow(markers(outliers(data))) > 0){
		rawData <- subset(merge(rawData, cbind(markers(outliers(data)), remove = TRUE), all.x = TRUE), is.na(remove))
		rawData$remove <- NULL
	}
	rawData$Raw <- eval(parse(text = sub("Fluorescence", "rawData$Fluorescence", data@model[2])))
	replicatedData <- merge(subset(rawData, Replicate %in% Repeated$Replicate), Repeated)
	Variances <- ddply(replicatedData, c("PC", "Marker", "Specimen"), function(z){
		with(z, c(Raw = var(Raw, na.rm = TRUE), Normalised = var(Normalised, na.rm = TRUE)))
	})
	MarkerDeviance <- recast(PC + Marker ~ variable, data = Variances, id.var = c("PC", "Marker"), measure.var = c("Raw", "Normalised"), function(z){mean(z, na.rm = TRUE)})
	SpecimenDeviance <- recast(PC + Specimen ~ variable, data = Variances, id.var = c("PC", "Specimen"), measure.var = c("Raw", "Normalised"), function(z){mean(z, na.rm = TRUE)})
	if(bootstrap){
		bootDeviance <- rlply(199, {
			bootSample <- replicatedData
			bootSample$Raw <- sample(replicatedData$Raw, replace = TRUE)
			bootSample$Normalised <- sample(replicatedData$Normalised, replace = TRUE)
			bootVariances <- ddply(bootSample, c("PC", "Marker", "Specimen"), function(z){
				with(z, c(Raw = var(Raw, na.rm = TRUE), Normalised = var(Normalised, na.rm = TRUE)))
			})
			list(Marker = recast(PC + Marker ~ variable, data = bootVariances, id.var = c("PC", "Marker"), measure.var = c("Raw", "Normalised"), function(z){mean(z, na.rm = TRUE)}),
				Specimen= recast(PC + Specimen ~ variable, data = bootVariances, id.var = c("PC", "Specimen"), measure.var = c("Raw", "Normalised"), function(z){mean(z, na.rm = TRUE)}))
		})
	}
	if(all(is.na(rawData$Normalised))){
		if(bootstrap){
			bootMarkerDeviance <- cast(PC + Marker ~ ., data = ldply(bootDeviance, function(x){x$Marker}), value = "Raw", quantile, prob = 0.95, type = 6)
			colnames(bootMarkerDeviance)[3] <- "Raw"
			MarkerDeviance <- merge(MarkerDeviance, bootMarkerDeviance, by = c("PC", "Marker"), suffixes = c("", ".y"))
			MarkerDeviance$Outlier <- factor(ifelse(with(MarkerDeviance, Raw > Raw.y), 1, 2), levels = 1:2, labels = c("Possible", "Acceptable"))
			bootSpecimenDeviance <- cast(PC + Specimen ~ ., data = ldply(bootDeviance, function(x){x$Specimen}), value = "Raw", quantile, prob = 0.95, type = 6)
			colnames(bootSpecimenDeviance)[2] <- "Raw"
			SpecimenDeviance <- merge(SpecimenDeviance, bootSpecimenDeviance, by = c("PC", "Specimen"), suffixes = c("", ".y"))
			SpecimenDeviance$Outlier <- factor(ifelse(with(SpecimenDeviance, Raw > Raw.y), 1, 2), levels = 1:2, labels = c("Possible", "Acceptable"))
			if(output != "none"){
				pMarker <- 
					ggplot(MarkerDeviance, aes(x = Raw, fill = Outlier)) + geom_histogram() + geom_rug() + facet_wrap(~PC)
				pSpecimen <- 
					ggplot(SpecimenDeviance, aes(x = Raw, fill = Outlier)) + geom_histogram() + geom_rug() + facet_wrap(~PC)
			}
			MarkerDeviance$Observed <- MarkerDeviance$Raw
			SpecimenDeviance$Observed <- SpecimenDeviance$Raw
			result <- AFLP.outlier(
				Marker = subset(MarkerDeviance, Outlier == "Possible")[, c("PC", "Marker", "Observed")],
				Specimen = subset(SpecimenDeviance, Outlier == "Possible")[, c("PC", "Specimen", "Observed")])
		} else {
			if(output != "none"){
				pMarker <- 
					ggplot(MarkerDeviance, aes(x = Raw)) + geom_histogram() + geom_rug() + facet_wrap(~PC)
				pSpecimen <- 
					ggplot(SpecimenDeviance, aes(x = Raw)) + geom_histogram() + geom_rug() + facet_wrap(~PC)
			}
			result <- AFLP.outlier()
		}
	} else {
		if(bootstrap){
			bootMarkerDeviance <- cast(PC + Marker ~ ., data = ldply(bootDeviance, function(x){x$Marker}), value = "Normalised", quantile, prob = 0.95, type = 6)
			colnames(bootMarkerDeviance)[3] <- "Normalised"
			MarkerDeviance <- merge(MarkerDeviance, bootMarkerDeviance, by = c("PC", "Marker"), suffixes = c("", ".y"))
			MarkerDeviance$Outlier <- factor(ifelse(with(MarkerDeviance, Normalised > Normalised.y), 1, 2), levels = 1:2, labels = c("Possible", "Acceptable"))
			bootSpecimenDeviance <- cast(Specimen ~ ., data = ldply(bootDeviance, function(x){x$Specimen}), value = "Normalised", quantile, prob = 0.95, type = 6)
			colnames(bootSpecimenDeviance)[2] <- "Normalised"
			SpecimenDeviance <- merge(SpecimenDeviance, bootSpecimenDeviance, by = c("PC", "Specimen"), suffixes = c("", ".y"))
			SpecimenDeviance$Outlier <- factor(ifelse(with(SpecimenDeviance, Normalised > Normalised.y), 1, 2), levels = 1:2, labels = c("Possible", "Acceptable"))
			if(output != "none"){
				pMarker <- 
					ggplot(MarkerDeviance, aes(x = Raw, y = Normalised, colour = Outlier)) + geom_point() + geom_abline() + facet_wrap(~PC)
				pSpecimen <- 
					ggplot(SpecimenDeviance, aes(x = Raw, y = Normalised, colour = Outlier)) + geom_point() + geom_abline() + facet_wrap(~PC)
			}
			MarkerDeviance$Observed <- MarkerDeviance$Normalised
			SpecimenDeviance$Observed <- SpecimenDeviance$Normalised
			result <- AFLP.outlier(
				Marker = subset(MarkerDeviance, Outlier == "Possible")[, c("PC", "Marker", "Observed")],
				Specimen = subset(SpecimenDeviance, Outlier == "Possible")[, c("PC", "Specimen", "Observed")])
		} else {
			if(output != "none"){
				pMarker <- 
					ggplot(MarkerDeviance, aes(x = Raw, y = Normalised)) + geom_point() + geom_abline() + facet_wrap(~PC)
				pSpecimen <- 
					ggplot(SpecimenDeviance, aes(x = Raw, y = Normalised)) + geom_point() + geom_abline() + facet_wrap(~PC)
			}
			result <- AFLP.outlier()
		}
	}
	if(output == "tex"){
		ggsave.latex(pMarker, caption = "Repeatability for markers based on fluorescence", filename = paste("QCMarkerFluorescence", device, sep = "."), width = 6, height = 2.5, path = path)
		if("Outlier" %in% colnames(MarkerDeviance)){
			print(xtable(MarkerDeviance[, colnames(MarkerDeviance) %in% c("PC", "Marker", "Raw", "Normalised", "Outlier")], caption = "Repeatability for markers based on fluorescence. Smaller is better.", align = "rrlllr", digits = c(0, 0, 0, 3, 3, 0), display = c("s", "s", "f", "f", "f", "s")), include.rownames = FALSE, tabular.environment = "longtable", floating = FALSE, size = "tiny")
		} else {
			print(xtable(MarkerDeviance[, colnames(MarkerDeviance) %in% c("PC", "Marker", "Raw", "Normalised")], caption = "Repeatability for markers based on fluorescence. Smaller is better.", align = "rrlll", digits = c(0, 0, 0, 3, 3), display = c("s", "s", "f", "f", "f")), include.rownames = FALSE, tabular.environment = "longtable", floating = FALSE, size = "tiny")
		}
		ggsave.latex(pSpecimen, caption = "Repeatability for specimens based on fluorescence", filename = paste("QCSpecimenFluorescence", device, sep = "."), width = 6, height = 2.5, path = path)
		if("Outlier" %in% colnames(SpecimenDeviance)){
			print(xtable(SpecimenDeviance[, colnames(SpecimenDeviance) %in% c("Specimen", "Raw", "Normalised", "Outlier")], caption = "Repeatability for specimens based on fluorescence. Smaller is better.", align = "rrllr", digits = c(0, 0, 3, 3, 0), display = c("s", "s", "f", "f", "s")), include.rownames = FALSE, tabular.environment = "longtable", floating = FALSE, size = "tiny")
		} else {
			print(xtable(SpecimenDeviance[, colnames(SpecimenDeviance) %in% c("Specimen", "Raw", "Normalised")], caption = "Repeatability for specimens based on fluorescence. Smaller is better.", align = "rrll", digits = c(0, 0, 3, 3), display = c("s", "s", "f", "f")), include.rownames = FALSE, tabular.environment = "longtable", floating = FALSE, size = "tiny")
		}
	} else if(output == "screen"){
		X11()
		print(pMarker + opts(title = "Repeatability for markers (fluorescence)"))
		cat("\nRepeatability for markers\n\n")
		print(MarkerDeviance[, colnames(MarkerDeviance) %in% c("PC", "Marker", "Raw", "Normalised", "Outlier")])
		X11()
		print(pSpecimen + opts(title = "Repeatability for specimens (fluorescence)"))
		cat("\nRepeatability for specimens\n\n")
		print(SpecimenDeviance[, colnames(SpecimenDeviance) %in% c("PC", "Specimen", "Raw", "Normalised", "Outlier")])
	}
	if(!all(is.na(replicatedData$Score))){
		Polymorph <- ddply(border(data), c("PC", "Marker"), function(z){
			c(Polymorph = all(is.finite(z$Border)))
		})
		replicatedData <- merge(replicatedData, Polymorph)
#		JaccardSpecimen <- na.omit(ddply(replicatedData, c("PC", "Specimen"), function(x){
#			y <- cast(data = x, PC + Marker ~ Replicate, value = "Score")
#			distance <- vegdist(t(y[, -1:-2]), method = "jaccard", na.rm = TRUE)
#			c(Min = min(distance), Mean = mean(distance), Max = max(distance), SD = sd(distance))
#		}))
#		DistanceSpecimen <- ddply(replicatedData, c("PC", "Specimen"), function(x){
#			y <- cast(data = x, PC + Marker ~ Replicate, value = "Normalised")[, -1:-2]
#			distance <- dist(y)
#			c(Min = min(distance), Mean = mean(distance), Max = max(distance), SD = sd(distance))
#		})

		replicatedData$Score <- factor(replicatedData$Score > 0, levels = c(FALSE, TRUE))
		repeatable <- ddply(replicatedData, c("PC", "Marker", "Specimen", "Polymorph"), function(x){
			table(x$Score)
		})
		repeatable$nBin <- rowSums(repeatable[, c("FALSE", "TRUE")])
		repeatable$Errors <- apply(repeatable[, c("FALSE", "TRUE")], 1, min)
		repeatable$MaxErrors <- floor(repeatable$nBin / 2)
		qcMarker <- ddply(repeatable, c("PC", "Marker", "Polymorph"), function(x){
			colSums(x[, c("Errors", "MaxErrors", "nBin")])
		})
		qcMarker$Score <- with(qcMarker, (MaxErrors - Errors) / MaxErrors)
		qcMarker <- qcMarker[with(qcMarker, order(Score, MaxErrors, PC, Marker)), ]
		quality(data, "marker") <- qcMarker[, c("PC", "Marker", "Score", "Errors", "MaxErrors", "nBin")]
		qcSpecimen <- ddply(subset(repeatable, Polymorph), c("PC", "Specimen"), function(x){
			colSums(x[, c("Errors", "MaxErrors", "nBin")])
		})
		qcSpecimen2 <- ddply(repeatable, c("PC", "Specimen"), function(x){
			colSums(x[, c("MaxErrors", "nBin")])
		})
		colnames(qcSpecimen2)[3:4] <- c("MaxErrorsAll", "nBinAll")
		qcSpecimen <- merge(qcSpecimen, qcSpecimen2)
		qcSpecimen$Score <- with(qcSpecimen, (MaxErrorsAll - Errors) / MaxErrorsAll)
		qcSpecimen <- qcSpecimen[with(qcSpecimen, order(Score, MaxErrors, Specimen)), ]
		quality(data, "specimen") <- qcSpecimen[, c("PC", "Specimen", "Score", "Errors", "MaxErrors", "nBin", "MaxErrorsAll", "nBinAll")]
		qcPC <- ddply(subset(repeatable, Polymorph), c("PC"), function(x){
			colSums(x[, c("Errors", "MaxErrors", "nBin")])
		})
		qcPC2 <- ddply(repeatable, c("PC"), function(x){
			colSums(x[, c("MaxErrors", "nBin")])
		})
		colnames(qcPC2)[2:3] <- c("MaxErrorsAll", "nBinAll")
		qcPC <- merge(qcPC, qcPC2)
		qcPC$Score <- with(qcPC, (MaxErrorsAll - Errors) / MaxErrorsAll)
		quality(data, "overall") <- qcPC[, c("PC", "Score", "Errors", "MaxErrors", "nBin", "MaxErrorsAll", "nBinAll")]
		qcPC$ScorePC <- sprintf("%.1f%%", 100 * qcPC$Score)
		qcSpecimen <- merge(qcSpecimen, qcPC[, c("PC", "ScorePC")])
		qcMarker <- merge(qcMarker, qcPC[, c("PC", "ScorePC")])
		
#		ggplot(qcSpecimen, aes(x = MaxDelta, y = Total)) + geom_smooth() + geom_point(aes(size = Delta))
#		ggplot(merge(qcSpecimen, JaccardSpecimen), aes(x = Score, y = Mean)) + geom_smooth(se = FALSE, span = 1) + geom_point() + scale_x_continuous("Herhaalbaarheids score") + scale_y_continuous("Gemiddelde Jaccard afstand")
		if(!is.null(minMarker)){
			if(bootstrap){
				warning("Outliers from bootstrap procedure overwritten")
			}
			tmpMarker <- subset(qcMarker, Score < minMarker)[, c("PC", "Marker", "Score")]
			colnames(tmpMarker)[3] <- "Observed"
			tmpSpecimen <- subset(qcSpecimen, Score < minMarker)[, c("PC", "Specimen", "Score")]
			colnames(tmpSpecimen)[3] <- "Observed"
			result <- AFLP.outlier(Marker = tmpMarker, Specimen = tmpSpecimen)
		}
		if(output != "none"){
			pMarker <- 
				ggplot(qcMarker, aes(x = Score)) + geom_histogram(binwidth = 0.025) + scale_x_continuous(limits = 0:1) + scale_y_continuous("Number of markers") + facet_wrap(~PC + ScorePC, scales = "free_y")
			pSpecimen <- 
				ggplot(qcSpecimen, aes(x = Score)) + geom_histogram(binwidth = 0.025) + scale_x_continuous(limits = 0:1) + scale_y_continuous("Number of specimens") + facet_wrap(~PC + ScorePC)
		}
		if(output == "tex"){
			ggsave.latex(pMarker, caption = "Repeatability for markers based on score", filename = paste("QCMarkerScore", device, sep = "."), width = 6, height = 2.5, path = path)
			print(xtable(qcMarker[, c("PC", "Marker", "Delta", "Total", "Score")], caption = "Repeatability for markers based on score", align = "rrllll", digits = c(0, 0, 0, 0, 0, 3), display = c("s", "s", "s", "f", "f", "f")), include.rownames = FALSE, tabular.environment = "longtable", floating = FALSE, size = "tiny")
			ggsave.latex(pSpecimen, caption = "Repeatability for specimens based on score", filename = paste("QCSpecimenScore", device, sep = "."), width = 6, height = 2.5, path = path)
			print(xtable(qcSpecimen[, c("PC", "Specimen", "Delta", "Total", "Score")], caption = "Repeatability for specimens based on score", align = "rrrllr", digits = c(0, 0, 0, 0, 0, 3), display = c("s", "s", "s", "f", "f", "f")), include.rownames = FALSE, tabular.environment = "longtable", floating = FALSE, size = "tiny")
		} else if (output == "screen"){
			X11()
			print(pMarker + opts(title = "Repeatability for markers (score)"))
			cat("\nRepeatability for markers\n\n")
			print(qcMarker)
			X11()
			print(pSpecimen + opts(title = "Repeatability for specimens (score)"))
			cat("\nRepeatability for specimens\n\n")
			print(qcSpecimen)
		}
	}
	invisible(list(data = data, Outliers = result))
}
