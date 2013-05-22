#'Classifies normalised AFLP data
#'
#'The normalised fluorescence score of AFLP data are converted into one or more
#'classes. The classification is based on the first and second derivative of
#'the density of the normalised fluorescence. Hence the data must be normalised
#'prior to classification.
#'
#'Monomorph markers will have only one class and the corresponding border with
#'have \code{Inf} as cut-level. The lowest class in polymorphic markers is
#'considered an 'absent' locus, all other classes indicate a 'present' locus.
#'
#'We strongly suggest that the user thoroughly inspects the resulting plots as
#'the automated classification might yield some unwanted artifacts. Correct
#'them with the \code{link{border<-}} function.
#'
#'
#'@param data A normalised AFLP object. Unnormalised object will be normalised
#'prior to classification.
#'@param output Indicates how the inspections plots should be given. "none"
#'suppresses them, "screen" prints them on the screen, "tex" save them to a
#'file and generates LaTeX to include these files into a document.
#'@param maxBorder The maximum number of borders per marker.
#'@param tresholdPeakRatio The minimum height of a potential peak in the
#'density relative to the maximum density. Peak with a ratio lower that this
#'threshold are not considered as peaks. Defaults to 0.03.
#'@param tresholdMonomorph Calculate the ratio of 'absent' and 'present' bins
#'in a marker. Consider the marker as monomorph if either of them is smaller
#'than the threshold. Defaults to 0.
#'@param tresholdMaxValley A treshold for valleys between peaks in the relative
#'density. P,my vallyes below this treshold are considered for breaks. Defaults
#'to 0.95.
#'@param path the path where the figures are saved. Only used if \code{output =
#'"tex"}.  Defaults to NULL, which is the working directory.
#'@param device the device to which the figures are saved. See
#'\code{\link[ggplot2]{ggsave}} for the available devices. Only used if
#'\code{output = "tex"}. Defaults to "pdf".
#'@param nrows Gives the prefer number of rows with plot per figure. Defaults
#'to 4.
#'@param ncols Gives the prefer number of columns with plot per figure.
#'Defaults to 4.
#'@param keep.border Logical. If FALSE then the borders are recalculated and
#'overwritten. If TRUE the borders in the object are used for the
#'classification. The latter is intended to update the classification after
#'manually altering the borders.  Default to FALSE.
#'@return Returns the \code{data} object with a updated \code{borders} slot and
#'updated \code{Score} in the \code{Fluorescence} slot.
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{border<-}}, \code{\link{border}},
#'\code{\link{normalise}}, \code{\link[ggplot2]{ggsave}}
#'@keywords manip classif
#'@examples
#'
#'  data(Tilia)
#'  Tilia <- classify(Tilia, output = "none")
#'
#'@export
#'@importFrom plyr ddply d_ply dlply is.formula
#'@importFrom ggplot2 ggplot geom_density geom_point scale_x_continuous scale_y_continuous facet_wrap geom_vline aes ggtitle

classify <- function(data, output = c("screen", "none", "tex"), maxBorder = 1, tresholdPeakRatio = 0.03, tresholdMonomorph = 0, tresholdMaxValley = .95, device = "pdf", path = NULL, nrows = 4, ncols = 4, keep.border = FALSE){
  # #####################
  # Fooling R CMD check #
  #######################
  Border <- Normalised <- Specimen <- NULL
  # #####################
  # Fooling R CMD check #
  #######################
  
  output <- match.arg(output)
	if(!is.AFLP(data)){
		stop("data must be an AFLP object.")
	}
	if(all(is.na(fluorescence(data)$Normalised))){
		warning("Data was normalised before classification.")
		data <- normalise(data, output = "screen")
	}
	Repeated <- replicates(data)[, c("Specimen", "Replicate", "Capilar", "Lane")]
	if(nrow(replicates(outliers(data))) > 0){
		Repeated <- subset(merge(Repeated, cbind(replicates(outliers(data)), remove = TRUE), all.x = TRUE), is.na(remove))[, c("Specimen", "Replicate", "Lane")]
	}
	if(nrow(specimens(outliers(data))) > 0){
		Repeated <- subset(merge(Repeated, cbind(specimens(outliers(data)), remove = TRUE), all.x = TRUE), is.na(remove))[, c("Specimen", "Replicate", "Lane")]
	}
  Repeated2 <- aggregate(Replicate ~ Specimen, data = Repeated, FUN = length)
	Repeated <- subset(Repeated, Specimen %in% Repeated2$Specimen[Repeated2[, 2] > 1])
	
	ExtraCols <- colnames(fluorescence(data))
	ExtraCols <- ExtraCols[!ExtraCols %in% c("PC", "Replicate", "Fluorescence", "Marker", "Normalised", "Score")]
	dataset <- fluorescence(data)
	dataset$Score <- NA
	xLimits <- range(dataset$Normalised, na.rm = TRUE)
	dataset$PCMarker <- paste(dataset$PC, round(dataset$Marker, 2), sep = "_")
	#x <- subset(dataset, PCMarker == "PC2_591")
	#x <- subset(dataset, PCMarker == "B_100.803076923077")
	result <- dlply(dataset, "PCMarker", function(x){
		if(keep.border){
			Border <- border(data, x$PC, x$Marker)
		} else {
			Dens <- density(na.omit(x$Normalised), n = 3000)
			Dens <- data.frame(Normalised = Dens$x, Density = Dens$y)
			Dens$First <- c(0, diff(Dens$Density))
			Dens$Second <- c(0, diff(Dens$First)) > 0
			Dens$Ratio <- Dens$Density / max(Dens$Density)
			Dens$Peak[abs(Dens$First) < 1e-4 & Dens$Second & Dens$Ratio <= tresholdMaxValley] <- "Valley"
			Dens$Peak[abs(Dens$First) < 1e-4 & !Dens$Second & Dens$Ratio >= tresholdPeakRatio] <- "Peak"
			Selection <- na.omit(Dens)
			Selection <- Selection[cumsum(Selection$Peak == "Peak") > 0, ]
			Selection <- Selection[rev(cumsum(rev(Selection$Peak == "Peak"))) > 0, ]
			if(sum(Selection$Peak == "Valley") > 0){
				Selection$Cumsum <- cumsum(Selection$Peak == "Peak")
				Selection <- ddply(Selection, "Cumsum", function(x){
					x <- x[x$Peak == "Valley", ]
					x[which.min(abs(x$First)), ]
				})
				if(is.null(Selection$Normalised)){
					Border <- Inf
				} else {
					Border <- Selection$Normalised
				}
			} else {
				Border <- Inf
			}
		}
		if(length(Border) > maxBorder){
			Border <- Border[order(borderProbability(Fluor = x, Borders = Border, Repeated = Repeated))][seq_len(maxBorder)]
		}
		if(length(Border) > 1){
			x$Score <- cut(x$Normalised, breaks = c(-Inf, Border, Inf), labels = seq_len(length(Border) + 1))
			x$Score <- as.numeric(levels(x$Score))[x$Score]
		} else {
			if(is.finite(Border)){
				x$Score <- ifelse(x$Normalised <= Border, 0, 1)
        if(min(table(x$Score)) / nrow(x) <= tresholdMonomorph){
          x$Score <- ifelse(is.na(x$Normalised), NA, ifelse(is.na(x$Sign) | x$Sign > 0, 1, 0))
        }
			} else {
			  x$Score <- ifelse(is.na(x$Normalised), NA, ifelse(is.na(x$Sign) | x$Sign > 0, 1, 0))
			}
		}
		list(Fluorescence = x[, c("PC", "Replicate", "Fluorescence", "Marker", "Normalised", "Score", ExtraCols)], Border = data.frame(PC = unique(x$PC), Marker = unique(x$Marker), Border = Border))
	})
	data@Fluorescence <- do.call("rbind", lapply(result, function(x){x$Fluorescence}))
	if(!keep.border){
		data@Borders <- do.call("rbind", lapply(result, function(x){x$Border}))
	}
	dataset <- fluorescence(data)
	nMarker <- ddply(dataset, "PC", function(x){
		c(Markers = length(unique(x$Marker)))
	})
	if(output == "screen"){
		if(sum(ceiling(nMarker$Markers / (nrows * ncols))) > 60){
			warning("ncol and nrow increased to reduce the number of open devices.")
			while(sum(ceiling(nMarker$Markers / (nrows * ncols))) > 60){
				nrows <- nrows + 1
				ncols <- ncols + 1
			}
		}
	}
	d_ply(dataset, "PC", function(x){
		Markers <- unique(x$Marker)
		Markers <- data.frame(PC = unique(x$PC), Marker = Markers, Group = ceiling(seq_along(Markers) / (nrows * ncols)))
		d_ply(Markers, "Group", function(z){
			y <- merge(x, z)
			Borders <- merge(border(data), z)
			if(output != "none"){
				pDens <- 
					ggplot(y, aes(x = Normalised)) + geom_density() + geom_point(aes(y = 0), size = 1) + scale_x_continuous("", limits = xLimits) + scale_y_continuous("Density of replicates") + facet_wrap(~Marker, ncol = ncols) + geom_vline(data = Borders, colour = "red", aes(xintercept = Border))
			}
			if(output == "tex"){
				caption <- paste("Density of normalised fluorescence and cut-off values per class for", unique(y$PC))
				filename <- paste("Dens", sub(":", "_", sub(":", "_", unique(y$PC))), "_", unique(y$Group), ".pdf", sep = "")
				ggsave.latex(pDens, caption = caption, filename = filename, path = path, width = 6, height = 6)
			} else if (output == "screen"){
				X11()
				print(pDens + ggtitle(paste("Density of normalised fluorescence for", unique(y$PC))))
			}
		})
		if(output == "tex"){
			cat("\\FloatBarrier\n\n")
		}
	})
	invisible(data)
}
