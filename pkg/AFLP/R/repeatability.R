#'Estimate the repeatability of AFLP data
#'
#'This function evaluates two indicators for the repeatability of the data: one
#'based on the fluoresence and one on the classification. The indicators are
#'based on all specimens with more than one replicate, outliers excluded. The
#'indicators are given for both the specimens as the markers.
#'
#'The indicator based on the fluorescence (raw or normalised) behaves like a
#'variance. 0 equals a perfect match between all replicates from the same
#'specimen. Higher values indicate less repeatable data. There is no upper
#'limit for this value. The value is only useful to compare specimen or marker
#'with the same project.
#'
#'The indicator based on the score ranges from 0 (not reproducible at all,
#'score is more or less random) to 1 (perfect repeatability).
#'
#'The calculation of both indicator is described in the Details section.
#'
#'First a selection is made from all possible combinations of marker and
#'specimen were data of more than one replicate is available. This selection is
#'used for both indicators.
#'
#'The indicator on the fluorescence (raw and normalised) starts by calculating
#'the variance of the fluorescence for each combination of specimen and marker.
#'If the data is repeatable then the fluorescence will be very similar and
#'hence the variance will be close to zero. The indicator per specimen is
#'simply the mean of these variances over all markers. Likewise we the mean per
#'marker of the variances over all specimens is the indicator per marker.
#'
#'The indicator on the score is based on the number of possible mistakes. First
#'the scores are converted into a binary score. The lowest class is considered
#''absent', all other classed 'present'. Then we look at the number of 'absent'
#'and 'present' replicates for each combination of marker and specimen. The
#'class with the highest number is presumed to be the correct class. Hence the
#'maximum number of mistakes for each combination of marker and specimen is the
#'number of replicates divide by 2 and rounded downward. Now we have for all
#'those combinations a number of mistakes and the maximum number of mistakes.
#'We calculate for each specimen the sum of both numbers over all markers. Then
#'we subtract the total number of mistakes from the total maximum number of
#'mistake and divide that by the total maximum number of mistakes. If all
#'replicates yield the same class, then no mistakes are made and the indicator
#'equals 1. If the data has a very bad repeatability and half of the replicates
#'are 'absent' and half 'present', then the total number of mistakes will equal
#'the total maximum number of mistakes. This leads to an indicator equal to 0.
#'The indicator per marker is calculated in a similar fashion (aggregation on
#'marker instead of specimen).
#'
#'@param data An AFLP object with at least raw fluoresence data.
#'@param output Which output is required. "screen" put graphics and possible
#'outliers on the screen. "tex" givens the same information but saves the
#'graphics to files and report LaTeX code to include the information in a LaTeX
#'document. "none" suppresses the output.
#'@param bootstrap Logical. Indicates whether a bootstrap procedure is run to
#'detect possible outliers. These are specimens or markers which are probably
#'unreliable because of a bad repeatability. Default to FALSE. Warning: setting
#'this to TRUE can require a long computing time.
#'@param minMarker All markers and specimens with a score less than
#'\code{minMarker} are put in AFLP.outlier object. Defaults to NULL.
#'@param path the path where the figures are saved. Only used if \code{output =
#'"tex"}.  Defaults to NULL, which is the working directory.
#'@param device the device to which the figures are saved. See
#'\code{\link[ggplot2]{ggsave}} for the available devices. Only used if
#'\code{output = "tex"}. Defaults to "pdf".
#'@return
#'\itemize{
#'  \item data An ALFP object were the quality data is appended to.
#'  \item Outliers An AFLP.outlier object with the possible outliers. This
#'is based on the bootstrap procedure. Hence using \code{boostrap = FALSE} will
#'result is an empty AFLP.outlier object.
#'}
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{normalise}}, \code{\link{classify}},
#'\code{\link[ggplot2]{ggsave}}
#'@keywords attribute
#'@export
#'@examples
#'
#'	data(Tilia)
#'	output <- repeatability(Tilia, output = "none")
#'
#'@importFrom ggplot2 ggplot geom_histogram geom_rug geom_point geom_abline facet_wrap aes scale_x_continuous
#'@importFrom plyr ddply rlply ldply d_ply
#'@importFrom reshape recast cast
repeatability <- function(data, output = c("screen", "tex", "none"), bootstrap = FALSE, minMarker = NULL, path = NULL, device = "pdf"){
  # #####################
  # Fooling R CMD check #
  #######################
  Freq <- Marker <- Normalised <- Outlier <- PC <- Plate <- PlateA <- PlateB <- Raw <- Replicate <- Score <- Specimen <- SpecimenPlate <- NULL
  # #####################
  # Fooling R CMD check #
  #######################
  output <- match.arg(output)
	Repeated <- replicates(data)[, c("Specimen", "Replicate", "Lane")]
	if(nrow(replicates(outliers(data))) > 0){
		Repeated <- subset(merge(Repeated, cbind(replicates(outliers(data)), remove = TRUE), all.x = TRUE), is.na(remove))[, c("Specimen", "Replicate", "Lane")]
	}
	if(nrow(specimens(outliers(data))) > 0){
		Repeated <- subset(merge(Repeated, cbind(specimens(outliers(data)), remove = TRUE), all.x = TRUE), is.na(remove))[, c("Specimen", "Replicate", "Lane")]
	}
  
	Repeated2 <- aggregate(Replicate ~ Specimen, data = Repeated, FUN = length)
	Repeated <- subset(Repeated, Specimen %in% Repeated2$Specimen[Repeated2[, 2] > 1])
	rawData <- fluorescence(data)
	if(nrow(markers(outliers(data))) > 0){
		rawData <- subset(merge(rawData, cbind(markers(outliers(data)), remove = TRUE), all.x = TRUE), is.na(remove))
		rawData$remove <- NULL
	}
	rawData$Raw <- eval(parse(text = sub("Fluorescence", "rawData$Fluorescence", data@model[2])))
	replicatedData <- merge(subset(rawData, Replicate %in% Repeated$Replicate), Repeated)
	replicatedData <- subset(replicatedData, Specimen %in% levels(replicatedData$Specimen)[rowSums(with(replicatedData, table(Specimen, Replicate)) > 1) > 1])
	Variances <- ddply(replicatedData, c("PC", "Marker", "Specimen"), function(z){
		with(z, c(Raw = var(Raw, na.rm = TRUE), Normalised = var(Normalised, na.rm = TRUE)))
	})
  MarkerDeviance <- aggregate(cbind(Raw, Normalised) ~ PC + Marker, data = Variances, FUN = mean, na.rm = TRUE)
# 	MarkerDeviance <- recast(PC + Marker ~ variable, data = Variances, id.var = c("PC", "Marker"), measure.var = c("Raw", "Normalised"), fun.aggregate = function(z){mean(z, na.rm = TRUE)})
  SpecimenDeviance <- aggregate(cbind(Raw, Normalised) ~ PC + Specimen, data = Variances, FUN = mean, na.rm = TRUE)
# 	SpecimenDeviance <- recast(PC + Specimen ~ variable, data = Variances, id.var = c("PC", "Specimen"), measure.var = c("Raw", "Normalised"),fun.aggregate = function(z){mean(z, na.rm = TRUE)})
	if(bootstrap){
		bootDeviance <- rlply(199, {
			bootSample <- replicatedData
			bootSample$Raw <- sample(replicatedData$Raw, replace = TRUE)
			bootSample$Normalised <- sample(replicatedData$Normalised, replace = TRUE)
			bootVariances <- ddply(bootSample, c("PC", "Marker", "Specimen"), function(z){
				with(z, c(Raw = var(Raw, na.rm = TRUE), Normalised = var(Normalised, na.rm = TRUE)))
			})
			list(Marker = recast(PC + Marker ~ variable, data = bootVariances, id.var = c("PC", "Marker"), measure.var = c("Raw", "Normalised"), fun.aggregate = function(z){mean(z, na.rm = TRUE)}),
				Specimen= recast(PC + Specimen ~ variable, data = bootVariances, id.var = c("PC", "Specimen"), measure.var = c("Raw", "Normalised"), fun.aggregate = function(z){mean(z, na.rm = TRUE)}))
		})
	}
	if(all(is.na(rawData$Normalised))){
		if(bootstrap){
			bootMarkerDeviance <- cast(PC + Marker ~ ., data = ldply(bootDeviance, function(x){x$Marker}), value = "Raw", fun.aggregate = quantile, prob = 0.95, type = 6)
			colnames(bootMarkerDeviance)[3] <- "Raw"
			MarkerDeviance <- merge(MarkerDeviance, bootMarkerDeviance, by = c("PC", "Marker"), suffixes = c("", ".y"))
			MarkerDeviance$Outlier <- factor(ifelse(with(MarkerDeviance, Raw > Raw.y), 1, 2), levels = 1:2, labels = c("Possible", "Acceptable"))
			bootSpecimenDeviance <- cast(PC + Specimen ~ ., data = ldply(bootDeviance, function(x){x$Specimen}), value = "Raw", fun.aggregate = quantile, prob = 0.95, type = 6)
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
		print(pMarker + ggtitle("Repeatability for markers (fluorescence)"))
		cat("\nRepeatability for markers\n\n")
		print(MarkerDeviance[, colnames(MarkerDeviance) %in% c("PC", "Marker", "Raw", "Normalised", "Outlier")])
		X11()
		print(pSpecimen + ggtitle("Repeatability for specimens (fluorescence)"))
		cat("\nRepeatability for specimens\n\n")
		print(SpecimenDeviance[, colnames(SpecimenDeviance) %in% c("PC", "Specimen", "Raw", "Normalised", "Outlier")])
	}
	if(!all(is.na(replicatedData$Score))){
		Polymorph <- ddply(border(data), c("PC", "Marker"), function(z){
			c(Polymorph = all(is.finite(z$Border)))
		})
		replicatedData <- merge(replicatedData, Polymorph)
		replicatedData <- merge(replicatedData, replicates(data)[, c("Replicate", "Plate")])
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
		quality(data, "primercombination") <- qcPC[, c("PC", "Score", "Errors", "MaxErrors", "nBin", "MaxErrorsAll", "nBinAll")]
		qcSpecimenInd <- ddply(replicatedData, c("PC", "Specimen"), function(x){
			Z <- data.frame(t(combn(levels(x$Replicate)[unique(x$Replicate)], 2)))
			colnames(Z) <- c("ReplicateA", "ReplicateB")
			Z <- cbind(Z, t(apply(Z, 1, function(reps){
				tmp <- subset(x, Replicate %in% reps, select = c("Marker", "Score"))
				c(Errors = sum(with(tmp, table(Marker, Score))[, 1] == 1), MaxErrors  = length(unique(x$Marker)))
			})))
			Z
		})
		qcSpecimenInd$Score <- 1 - qcSpecimenInd$Errors / qcSpecimenInd$MaxErrors
		quality(data, "replicate") <- qcSpecimenInd
		qcPlate <- ddply(replicatedData, .(PC), function(x){
			Z <- subset(expand.grid(PlateA = unique(x$Plate), PlateB = unique(x$Plate)), as.numeric(PlateA) <= as.numeric(PlateB))
			Z <- cbind(Z, 
				t(apply(Z, 1, function(reps){
					tmpA <- subset(x, Plate == reps[1], select = c("Marker", "Specimen", "Score"))
					tmpB <- subset(x, Plate == reps[2], select = c("Marker", "Specimen", "Score"))
					tmpA <- tmpA[tmpA$Specimen %in% tmpB$Specimen, ]
					tmpB <- tmpB[tmpB$Specimen %in% tmpA$Specimen, ]
					tmp <- rbind(tmpA, tmpB)
					tmp <- ddply(tmp, .(Marker, Specimen), function(z){
						data.frame(Errors = min(table(z$Score)), MaxErrors = floor(nrow(z) / 2))
					})
					c(Score = 1 - sum(tmp$Errors) / sum(tmp$MaxErrors), Errors = sum(tmp$Errors), MaxErrors = sum(tmp$MaxErrors))
				}))
			)
			Z
		})
		quality(data, "plate") <- qcPlate
		tmp <- ddply(replicatedData, .(PC, Marker, Specimen), function(z){
			data.frame(Errors = min(table(z$Score)), MaxErrors = floor(nrow(z) / 2))
		})
		qcGlobal <- data.frame(Type = "Global", Score = 1 - sum(tmp$Errors) / sum(tmp$MaxErrors), Errors = sum(tmp$Errors), MaxErrors = sum(tmp$MaxErrors))
		tmp <- unique(replicatedData[, c("Specimen", "Replicate", "Plate")])
		Design <- with(tmp, table(Specimen, Plate))
		Selection <- subset(as.data.frame(Design), Freq > 1)
		Selection$SpecimenPlate <- paste(Selection$Specimen, Selection$Plate, sep = ":")
		replicatedData$SpecimenPlate <- paste(replicatedData$Specimen, replicatedData$Plate, sep = ":")
		tmp <- ddply(subset(replicatedData, SpecimenPlate %in% Selection$SpecimenPlate), .(PC, Marker, Specimen), function(z){
			data.frame(Errors = min(table(z$Score)), MaxErrors = floor(nrow(z) / 2))
		})
		qcGlobal <- rbind(qcGlobal, data.frame(Type = "Within plates", Score = 1 - sum(tmp$Errors) / sum(tmp$MaxErrors), Errors = sum(tmp$Errors), MaxErrors = sum(tmp$MaxErrors)))
		quality(data, "global") <- qcGlobal
		
		qcPC$ScorePC <- sprintf("%.1f%%", 100 * qcPC$Score)
		qcSpecimen <- merge(qcSpecimen, qcPC[, c("PC", "ScorePC")])
		qcMarker <- merge(qcMarker, qcPC[, c("PC", "ScorePC")])
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
			qcMarker$Polymorph <- qcMarker$Polymorph == 1
			print(
				xtable(qcMarker[, c("PC", "Marker", "Polymorph", "Score", "Errors", "MaxErrors", "nBin")], 
					caption = "Repeatability for markers based on score", 
					align = "rrlrllll", 
					digits = c(0, 0, 0, 0, 3, 0, 0, 0), 
					display = c("s", "s", "f", "d", "f", "d", "d", "d"))
			, include.rownames = FALSE, tabular.environment = "longtable", floating = FALSE, size = "tiny")
			ggsave.latex(pSpecimen, caption = "Repeatability for specimens based on score", filename = paste("QCSpecimenScore", device, sep = "."), width = 6, height = 2.5, path = path)
			print(
				xtable(qcSpecimen[, c("PC", "Specimen", "Score", "Errors", "MaxErrors", "nBin", "MaxErrorsAll", "nBinAll")], 
					caption = "Repeatability for specimens based on score", 
					align = "rrrllllll", 
					digits = c(0, 0, 0, 3, 0, 0, 0, 0, 0), 
					display = c("s", "s", "s", "f", "d", "d", "d", "d", "d"))
			, include.rownames = FALSE, tabular.environment = "longtable", floating = FALSE, size = "tiny")
			print(
				xtable(qcSpecimenInd[, c("PC", "Specimen", "ReplicateA", "ReplicateB", "Score", "Errors", "MaxErrors")], 
					caption = "Repeatability for replicates based on score", 
					align = "rrrrrlll", 
					digits = c(0, 0, 0, 0, 0, 3, 0, 0), 
					display = c("s", "s", "s", "s", "s", "f", "d", "d"))
			, include.rownames = FALSE, tabular.environment = "longtable", floating = FALSE, size = "tiny")
			print(
				xtable(qcPlate, 
					caption = "Repeatability for plates based on score", 
					align = "rrrrlll", 
					digits = c(0, 0, 0, 0,  3, 0, 0), 
					display = c("s", "s", "s", "s", "f", "d", "d"))
			, include.rownames = FALSE, tabular.environment = "longtable", floating = FALSE, size = "tiny")
			print(
				xtable(qcPlate, 
					caption = "Repeatability for plates based on score", 
					align = "rrrrlll", 
					digits = c(0, 0, 0, 0,  3, 0, 0), 
					display = c("s", "s", "s", "s", "f", "d", "d"))
			, include.rownames = FALSE, tabular.environment = "longtable", floating = FALSE, size = "tiny")
		} else if (output == "screen"){
			X11()
			print(pMarker + ggtitle("Repeatability for markers (score)"))
			cat("\nRepeatability for markers\n\n")
			print(qcMarker)
			X11()
			print(pSpecimen + ggtitle("Repeatability for specimens (score)"))
			cat("\nRepeatability for specimens\n\n")
			print(qcSpecimen)
			d_ply(qcSpecimenInd, .(PC, Specimen), function(Z){
				cat("\r\nPC: ", levels(Z$PC)[Z$PC[1]], ", Specimen: ", levels(Z$Specimen)[Z$Specimen[1]], "\r\n", sep = "")
				print(cast(as.formula("ReplicateA ~ ReplicateB"), data = Z, value = "Score", fill = "", fun.aggregate =function(x){ifelse(is.na(x), NA, sprintf("%0.3f", x))}))
			})
			d_ply(qcPlate, "PC", function(Z){
				cat("\r\nPC: ", levels(Z$PC)[Z$PC[1]], "\r\n", sep = "")
				print(cast(formula = PlateA ~ PlateB, data = Z, value = "Score", fill = "", fun.aggregate =function(x){ifelse(is.na(x), NA, sprintf("%0.3f", x))}))
			})
		}
	}
	invisible(list(data = data, Outliers = result))
}
