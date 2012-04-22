clean <- function(data){
	if(!require(reshape)){
		stop("This function requires the reshape package")
	}
	if(!is.AFLP(data)){
		stop("This check is only useful on ALFP objects")
	}
	missingPosition <- sum(apply(replicates(data)[, c("Plate", "Capilar", "Lane", "Replicate", "Specimen")], 1, function(z){
		sum(is.na(z)) > 0
	}))
	if(missingPosition > 0){
		warning(missingPosition, " replicate(s) omitted because of missing data.")
	}
	if(!all(specimens(data)$Specimen %in% replicates(data)$Specimen)) stop("Missing specimens in replicates")
	if(!all(replicates(data)$Specimen %in% specimens(data)$Specimen)) stop("Missing specimens in replicates")
	dataset <- merge(replicates(data), fluorescence(data))
	if(!all(dataset$Replicate %in% replicates(data)$Replicate)) stop("Missing replicates in fluorescence")
	if(!all(replicates(data)$Replicate %in% unique(dataset$Replicate))){
		cat("Replicates without fluoresence data\r\n")
		cat(levels(replicates(data)$Replicate)[unique(replicates(data)$Replicate[!replicates(data)$Replicate %in% unique(dataset$Replicate)])])
		warning("Replicates without fluoresence data")
	} 
	if(nrow(replicates(outliers(data))) > 0){
		dataset <- subset(merge(dataset, cbind(replicates(outliers(data)), remove = TRUE), all.x = TRUE), is.na(remove))
		dataset$remove <- NULL
		dataset$Observed <- NULL
	}
	if(nrow(specimens(outliers(data))) > 0){
		dataset <- subset(merge(dataset, cbind(specimens(outliers(data)), remove = TRUE), all.x = TRUE), is.na(remove))
		dataset$remove <- NULL
		dataset$Observed <- NULL
	}
	if(nrow(markers(outliers(data))) > 0){
		dataset <- subset(merge(dataset, cbind(markers(outliers(data)), remove = TRUE), all.x = TRUE), is.na(remove))
		dataset$remove <- NULL
		dataset$Observed <- NULL
	}
	if(nrow(residuals(outliers(data))) > 0){
		dataset <- subset(merge(dataset, cbind(residuals(outliers(data)), remove = TRUE), all.x = TRUE), is.na(remove))
		dataset$remove <- NULL
		dataset$Observed <- NULL
	}
	dataset$Missing <- ifelse(is.na(dataset$Fluorescence), "M", "A")
	Outliers <- list()
  Counts <- table(interaction(dataset$PC, dataset$Specimen, drop = TRUE), dataset$Missing)
  if(any(Counts[, 1] == 0)){
    sOutliers <- unique(dataset[, c("PC", "Specimen")])
    sOutliers$Observed <- NA
    sOutliers$Comb <- interaction(sOutliers$PC, sOutliers$Specimen, drop = TRUE)
    Outliers[["Specimen"]] <- sOutliers[sOutliers$Comb %in% rownames(Counts)[Counts[, 1] == 0], c("PC", "Specimen", "Observed")]
  } else {
    Outliers[["Specimen"]] <- dataset[NULL, c("PC", "Specimen")]
    Outliers[["Specimen"]]$Observed <- numeric(0)
  }
	Counts <- table(interaction(dataset$PC, dataset$Replicate, drop = TRUE), dataset$Missing)
	if(any(Counts[, 1] == 0)){
	  sOutliers <- unique(dataset[, c("PC", "Replicate")])
	  sOutliers$Observed <- NA
	  sOutliers$Comb <- interaction(sOutliers$PC, sOutliers$Replicate, drop = TRUE)
	  Outliers[["Replicate"]] <- sOutliers[sOutliers$Comb %in% rownames(Counts)[Counts[, 1] == 0], c("PC", "Replicate", "Observed")]
	} else {
	  Outliers[["Replicate"]] <- dataset[NULL, c("PC", "Replicate")]
	  Outliers[["Replicate"]]$Observed <- numeric(0)
	}
	Counts <- table(interaction(dataset$PC, dataset$Marker, drop = TRUE), dataset$Missing)
	if(any(Counts[, 1] == 0)){
	  sOutliers <- unique(dataset[, c("PC", "Marker")])
	  sOutliers$Observed <- NA
	  sOutliers$Comb <- interaction(sOutliers$PC, sOutliers$Marker, drop = TRUE)
	  Outliers[["Marker"]] <- sOutliers[sOutliers$Comb %in% rownames(Counts)[Counts[, 1] == 0], c("PC", "Marker", "Observed")]
	} else {
	  Outliers[["Marker"]] <- dataset[NULL, c("PC", "Marker")]
	  Outliers[["Marker"]]$Observed <- numeric(0)
	}
	Outliers[["Residuals"]] <- dataset[is.na(dataset$Fluorescence), c("PC", "Replicate", "Marker")]
	Outliers[["Residuals"]]$Observed <- rep(NA, nrow(Outliers[["Residuals"]]))
	data <- addOutliers(data, AFLP.outlier(Specimen = Outliers[["Specimen"]], Replicate = Outliers[["Replicate"]], 
		Marker = Outliers[["Marker"]], Residual = Outliers[["Residuals"]]))
  if(any(table(dataset$PC, dataset$Replicate, dataset$Marker) > 1)){
    warning("Replicates with multiple records per marker detected.")
  }
#	Fluor$PCMarker <- with(Fluor, factor(paste(PC, Marker)))
#	Peaks <- data.frame(with(Fluor, table(Replicate, PCMarker)))
#	if(sum(Peaks$Freq > 1) > 0){
#		warning("Some replicates have multiple records per marker. Only the strongest is retained.")
#		Multis <- subset(Peaks, Freq > 1)
#		Multis <- apply(Fluor[, c("Replicate", "PCMarker")], 1, function(x){
#			sum(Multis$Replicate == x["Replicate"] & Multis$PCMarker == x["PCMarker"]) > 0
#		})
#		Fluor <- rbind(
#			Fluor[!Multis, ],
#			ddply(Fluor[Multis, ], .(Replicate, PCMarker), function(x){
#				x[which.max(x$Fluorescence), ]
#			})
#		)
#	}
#	if(sum(Peaks$Freq == 0) > 0){
#		warning("Some replicates have no records for some markers. Appended with 80% of the lowest fluorescence.")
#		Missings <- subset(Peaks, Freq == 0)
#		newValue <- 0.8 * min(Fluor$Fluorescence)
#		Fluor <- rbind(Fluor, 
#			ddply(Missings, .(PCMarker), function(x){
#				data.frame(
#					Replicate = x$Replicate,
#					PC = Fluor$PC[Fluor$PCMarker == x$PCMarker[1]][1],
#					Marker = Fluor$Marker[Fluor$PCMarker == x$PCMarker[1]][1],
#					Fluorescence = newValue,
#					Normalised = NA,
#					Score = NA,
#					PCMarker = x$PCMarker
#				)
#			})
#		)
#	}
#	Fluor$PCMarker <- NULL
	return(data)
}
