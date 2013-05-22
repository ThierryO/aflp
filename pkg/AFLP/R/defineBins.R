#'Define bins in AFLP object
#'
#'This function is only relevant when the AFLP object contains raw data of peak
#'values.
#'
#'The peaks are aggregated into bins according to the algoritme described by
#'Arrigo et al (2009) \cite{Arrigo2009}.
#'
#'
#'@param dataset An AFLP object
#'@param minPeakHeight If set, only peaks with at least this amount of RFU will
#'be taken into account when defining bins.
#'@param minBinWidth Bins wider that this minimum AND who have at least one
#'replicate with more than one peak in that bin, will be split into to smaller
#'bins.
#'@param maxBinWidth If a bin is wider than this number of basepairs, then the
#'algorithm will split the bin, regardless the number of peaks within each
#'replicate.
#'@param missingPeakRatio If no peak is found for a give bin and given
#'replicate, then a peak value will be given that is equal to mssingPeakRatio
#'times the lowest peak value in the dataset.
#'@return A new AFLP object were the fluorescence slot is replaced by the bins.
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{normalise}}, \code{\link{readABI}}
#'@references Arrigo, N., Tuszynski, J., Ehrich, D., Gerdes, T. and Alvarez, N.
#'(2009), Evaluating the impact of scoring parameters on the structure of
#'intra-specific genetic variation using RawGeno, an R package for automating
#'AFLP scoring.  BMC Bioinformatics, 10:33, 10.1186/1471-2105-10-33,
#'\url{http://www.biomedcentral.com/1471-2105/10/33}
#'@keywords manip
#'@importFrom reshape cast
#'@export
defineBins <- function(dataset, minPeakHeight, minBinWidth = 1, maxBinWidth = 5, missingPeakRatio = 0.8){
  # #####################
  # Fooling R CMD check #
  #######################
  Bin <- Noise <- PC <- Replicate <- Specimen <- NULL
  # #####################
  # Fooling R CMD check #
  #######################
  
  if(missing(minPeakHeight)){
		fluorescence(dataset)$Noise <- 0
	} else {
		fluorescence(dataset)$Noise <- minPeakHeight
	}
	QCReplicates <- subset(replicates(dataset), Specimen %in% QC(dataset)$Specimen$Specimen | Replicate %in% QC(dataset)$Replicate$Replicate)$Replicate
	#Fluorescence <- subset(fluorescence(dataset), !(Replicate %in% QCReplicates) & PC == "O")
	Breaks <- ddply(subset(fluorescence(dataset), !(Replicate %in% QCReplicates)), .(PC), function(Fluorescence){
		Breaks <- c(0, max(Fluorescence$Marker) + minBinWidth)
		Fluorescence$Bin <- cut(Fluorescence$Marker, breaks = Breaks)
		BinWidth <- diff(Breaks)
		toSplit <- levels(Fluorescence$Bin)[BinWidth > 5]
		noSplit <- c()
		while(length(toSplit) > 0){
			#x <- subset(Fluorescence, Bin == sample(toSplit, 1) & Fluorescence >= Noise)
			extraBreaks <- ddply(subset(Fluorescence, Bin %in% toSplit & Fluorescence >= Noise), .(Bin), function(x){
				Peaks <- sort(unique(x$Marker))
				if(length(Peaks) > 1){
					dPeaks <- diff(Peaks)
					wm <- which.max(dPeaks)
					c(newBorder = mean(Peaks[wm + 0:1]))
				} else {
					c(newBorder = NA)
				}
			})
			Breaks <- sort(c(Breaks, extraBreaks$newBorder))
			noSplit <- c(noSplit, levels(extraBreaks$Bin)[extraBreaks$Bin[is.na(extraBreaks$newBorder)]])
			Fluorescence$Bin <- cut(Fluorescence$Marker, breaks = Breaks, dig.lab = 7)
			BinWidth <- diff(Breaks)
			MultiplePeaks <- apply(cast(Bin ~ Replicate, value = "Marker", data = Fluorescence, fun.aggregate = length, subset = Fluorescence >= Noise), 1, max) > 1
			toSplit <- levels(Fluorescence$Bin)[(MultiplePeaks & BinWidth > minBinWidth) | (BinWidth > maxBinWidth)]
			toSplit <- toSplit[!(toSplit %in% noSplit)]
		}
		data.frame(Breaks = Breaks)
	})
	#Fluorescence <- subset(fluorescence(dataset), PC == "O")
	fluorescence(dataset) <- ddply(fluorescence(dataset), .(PC), function(Fluorescence){
		Fluorescence$Bin <- cut(Fluorescence$Marker, breaks = subset(Breaks, PC == Fluorescence$PC[1])$Breaks)
		Fluorescence$Marker <- ave(Fluorescence$Marker, Fluorescence$Bin, FUN = mean)
		minFluorescence <- missingPeakRatio * min(Fluorescence$Fluorescence)
#		x <- subset(Fluorescence, Bin == sample(levels(Bin), 1))
		Fluorescence <- ddply(Fluorescence, .(Bin), function(x){
			Observed <- data.frame(unique(x[, c("PC", "Replicate", "Marker")]), Fluorescence = max(x$Fluorescence), Normalised = NA, Score = NA)
			MissingPeaks <- 
				data.frame(
					PC = Observed$PC[1],
					Replicate = levels(Fluorescence$Replicate)[!levels(Fluorescence$Replicate) %in% Observed$Replicate],
					Marker = Observed$Marker[1],
					Fluorescence = minFluorescence,
					Normalised = NA,
					Score = NA
				)
			rbind(Observed, MissingPeaks)
		})
		Fluorescence$Bin <- NULL
		Fluorescence
	})
	dataset
}

