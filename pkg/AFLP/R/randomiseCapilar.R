randomiseCapilar <- function(Specimens, Group, FirstLabID = 1, Prefix = "", nCapilar = 8, nLines = 12, QC, rReplicates = 0.1, minReplicates = 8, fillPlate = FALSE){
	if(is.numeric(Specimens)){
		Specimens <- seq_len(Specimens)
	}
	if(missing(Group)){
		specList <- data.frame(Specimen = Specimens, Group = NA)
	} else {
		specList <- data.frame(Specimen = Specimens, Group = Group)
	}
	if(missing(QC)){
		QC <- data.frame()
		specList$Specimen <- factor(specList$Specimen)
	} else {
		specList <- rbind(specList, data.frame(Specimen = QC$ID, Group = QC$Type))
		specList$Specimen <- factor(specList$Specimen)
		QC$ID <- factor(QC$ID, levels = levels(specList$Specimen))
	}
	Specimens <- factor(Specimens, levels = levels(specList$Specimen))
	nPlate <- ceiling(length(Specimens) / ((nCapilar * nLines - nrow(QC)) * (1 - rReplicates)))
	Design <- expand.grid(Capilar = LETTERS[seq_len(nCapilar)], Line = seq_len(nLines), Plate = seq_len(nPlate), Specimen = factor(NA, levels = levels(Specimens)), Replicate = NA)
	if(nrow(QC) > 0){
		Design <- merge(Design, QC, all.x = TRUE)
		Design$Replicate <- with(Design, ifelse(is.na(ID), NA, paste(ID, Plate, sep = "_")))
		Design$Specimen <- Design$ID
		Design$ID <- NULL
		Design$Type <- NULL
	}
	if(!fillPlate){
		lastLines <- ceiling((
			length(Specimens) + 
			pmax(ceiling(length(Specimens) * (1 / (1 - rReplicates) - 1)), minReplicates) + 
			nPlate * nrow(QC)
		) / nCapilar) %% nLines
		if(lastLines > 0){
			toReposition <- Design[Design$Plate == nPlate & !is.na(Design$Replicate) & Design$Line > lastLines, ]
			Design <- subset(Design, Plate < nPlate | Line <= lastLines)
			if(nrow(toReposition) > 0){
				toReposition$Line <- lastLines
				while(nrow(toReposition) > 0){
					x <- head(toReposition, 1)
					if(is.na(Design$Replicate[with(Design, Capilar == x$Capilar & Line == x$Line & Plate == nPlate)])){
						Design$Replicate[with(Design, Capilar == x$Capilar & Line == x$Line & Plate == nPlate)] <- x$Replicate
						Design$Specimen[with(Design, Capilar == x$Capilar & Line == x$Line & Plate == nPlate)] <- x$Specimen
						toReposition <- tail(toReposition, -1)
					} else {
						if(toReposition$Line[1] > 1){
							toReposition$Line[1] <- toReposition$Line[1] - 1
						} else {
							toReposition$Line[1] <- lastLines
							toReposition$Capilar[1] <- sample(Design$Capilar[Design$Plate == nPlate & is.na(Design$Replicate)], 1)
						}
					}
				}
			}
		}
	}
	QualityControl <- subset(Design, !is.na(Replicate))
	Design <- subset(Design, is.na(Replicate))
	Design <- Design[with(Design, order(Plate, Line, Capilar)), ]
	Design$Replicate<- sprintf("%s%04i", Prefix, seq_len(nrow(Design)) + FirstLabID - 1)
	Specimens <- sample(Specimens)
	while(sum(is.na(Design$Specimen)) - length(Specimens) >= 4){
		Design$Prob <- 
			(
				table(subset(Design, is.na(Specimen))$Plate)[Design$Plate] / table(Design$Plate)[Design$Plate]
			* 
				table(subset(Design, is.na(Specimen))$Capilar)[Design$Capilar] / table(Design$Capilar)[Design$Capilar]
			) ^ 10
		#add replicate within plate and capilar
		Remain <- subset(cast(Plate + Capilar ~ ., data = Design, subset = is.na(Specimen), value = "Prob", fun = c(length, mean)), length >= 2)
		i <- Remain[sample(seq_len(nrow(Remain)), 1, prob = Remain$mean), 1:2]
		lines <- sample(subset(Design, is.na(Specimen) & Plate == i$Plate & Capilar == i$Capilar)$Line, 2)
		Design$Specimen[with(Design, Plate == i$Plate & Capilar == i$Capilar & Line %in% lines)] <- Specimens[1]
		Design$Prob <- 
			(
				table(subset(Design, is.na(Specimen))$Plate)[Design$Plate] / table(Design$Plate)[Design$Plate]
			* 
				table(subset(Design, is.na(Specimen))$Capilar)[Design$Capilar] / table(Design$Capilar)[Design$Capilar]
			) ^ 10
		#add replicate within plate and between capilar
		Remain <- subset(Design, is.na(Specimen) & Plate == i$Plate & Capilar != i$Capilar)
		j <- Remain[sample(seq_len(nrow(Remain)), 1, prob = Remain$mean), 1:3]
		Design$Specimen[with(Design, Plate == j$Plate & Capilar == j$Capilar & Line == j$Line)] <- Specimens[1]
		Design$Prob <- 
			(
				table(subset(Design, is.na(Specimen))$Plate)[Design$Plate] / table(Design$Plate)[Design$Plate]
			* 
				table(subset(Design, is.na(Specimen))$Capilar)[Design$Capilar] / table(Design$Capilar)[Design$Capilar]
			) ^ 10
		#add replicate between plate and within capilar
		Remain <- subset(Design, is.na(Specimen) & Plate != i$Plate & Capilar == i$Capilar)
		k <- Remain[sample(seq_len(nrow(Remain)), 1, prob = Remain$mean), 1:3]
		Design$Specimen[with(Design, Plate == k$Plate & Capilar == k$Capilar & Line == k$Line)] <- Specimens[1]
		Design$Prob <- 
			(
				table(subset(Design, is.na(Specimen))$Plate)[Design$Plate] / table(Design$Plate)[Design$Plate]
			* 
				table(subset(Design, is.na(Specimen))$Capilar)[Design$Capilar] / table(Design$Capilar)[Design$Capilar]
			) ^ 10
		#add replicate between plate and between capilar
		Remain <- subset(Design, is.na(Specimen) & Plate != i$Plate & Capilar != i$Capilar)
		m <- Remain[sample(seq_len(nrow(Remain)), 1, prob = Remain$Prob), ]
		Design$Specimen[with(Design, Plate == m$Plate & Capilar == m$Capilar & Line == m$Line)] <- Specimens[1]
		Specimens <- Specimens[-1]
	}
	if(sum(is.na(Design$Specimen)) > length(Specimens)){
		extraN <- sum(is.na(Design$Specimen)) - length(Specimens)
		Design$Specimen[sample(which(is.na(Design$Specimen)), extraN)] <- sample(Specimens, extraN)
	}
	Design$Specimen[is.na(Design$Specimen)] <- sample(Specimens)
	Design$Prob <- NULL
	Design <- rbind(Design, QualityControl)
	Design$Lane <- factor(Design$Line)
	Design$Specimen <- factor(Design$Specimen)
	Design$Replicate <- factor(Design$Replicate)
	Design$Plate <- factor(Design$Plate)
	Design <- Design[with(Design, order(Plate, Lane, Capilar)), ]
	if(nrow(QC) > 0){
		return(
			new("AFLP",
				Specimens = specList,
				Replicates = Design[, c("Replicate", "Specimen", "Plate", "Capilar", "Lane")],
				QC = list(
					Specimens = data.frame(Specimen = QC$ID, Type = QC$Type),
					Replicates = merge(data.frame(Specimen = QC$ID, Type = QC$Type), Design)[, c("Replicate", "Type")]
				)
			)
		)
	} else {
		return(
			new("AFLP",
				Specimens = specList,
				Replicates = Design[, c("Replicate", "Specimen", "Plate", "Capilar", "Lane")]
			)
		)
	}
}
