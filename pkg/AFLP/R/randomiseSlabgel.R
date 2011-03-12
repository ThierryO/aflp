randomiseSlabgel <- function(Specimens, Group = NULL, nReplicates = 3, nQC = 0, nLanes = 64){
	nReplicates <- as.integer(nReplicates)
	nQC <- as.integer(nQC)
	nLanes <- as.integer(nLanes)
	if(nLanes < 1 | is.na(nLanes)){
		stop("A plate must contain at least one lane.\n")
	}
	if(nReplicates < 0){
		stop("The number of replicates must be positive.\n")
	}
	if(nReplicates  * 3 > nLanes){
		stop("The number of lanes must be larger than three times the number of replicates.\n")
	}
	if(nQC < 0){
		stop("The number of quality control specimens must be positive.\n")
	}
	if(nQC + 3 * nReplicates > nLanes){
		stop("Not enough room for the quality control specimens. Increase the number of lanes or decrease the number of replicates or quality control specimen.\n")
	}
	if(is.numeric(Specimens) && length(Specimens) == 1){
		Specimens <- as.integer(Specimens)
		if(Specimens < 1){
			stop("The number of specimens must be at least one.\n")
		}
		Specimens <- seq_len(Specimens)
	} else {
		if(anyDuplicated(Specimens)){
			stop("All specimen must have a unique name.\n")
		}
	}
	Specimens <- factor(Specimens)
	#create a dataframe to hold the choosen gels and randomize the specimens
	x <- data.frame(Specimen = sample(Specimens), Plate = NA, WithinGel = NA, BetweenGel = NA)
	#calculate the number of gels needed
	nGels <- ceiling(nrow(x) / (nLanes - 2 * nReplicates - nQC))
	#select the specimen for the within gel replicates
	whichGel <- rep(seq_len(nGels), nReplicates)
	x$Plate[seq_along(whichGel)] <- whichGel
	x$WithinGel[seq_along(whichGel)] <- whichGel
	#add between gel replicates for the specimens with within replicates
	if(nGels > 1){
		for(i in seq_along(whichGel)){
			tabPossible <- data.frame(table(x$WithinGel[x$WithinGel != x$WithinGel[i]]))
			tabDone <- data.frame(table(x$BetweenGel))
			if(nrow(tabDone) > 0){
				tabPossible <- merge(tabPossible, tabDone, all.x = TRUE, by = "Var1")
				tabPossible$Freq.y[is.na(tabPossible$Freq.y)] <- 0
				tabPossible$Freq <- tabPossible$Freq.x - tabPossible$Freq.y + 0.000001
			}
			if(nrow(tabPossible) > 1){
				x$BetweenGel[i] <- sample(x = as.numeric(levels(tabPossible$Var1)[tabPossible$Var1]), size = 1, prob = tabPossible$Freq)
			} else {
				x$BetweenGel[i] <- as.numeric(levels(tabPossible$Var1))
			}
		}
	} else {
		x$BetweenGel <- x$WithinGel
	}
	#add the ordinairy specimens to the gels
	whichToDo <- which(is.na(x$Plate))
	x$Plate[whichToDo] <- rep(seq_len(nGels), ceiling(nrow(x) / nGels))[seq_along(whichToDo)]
	#fill the gels with replicates. chooses non-replicated specimens first
	while(nGels * (nLanes - nQC) - sum(!is.na(x[, 2:4])) > 0 & sum(is.na(x$BetweenGel)) + sum(is.na(x$WithinGel)) > 0){
		tabPossible <- data.frame(table(na.omit(c(x$Plate, x$WithinGel, x$BetweenGel))))
		tabPossible$Freq <- nLanes - nQC - tabPossible$Freq
		tabPossible <- tabPossible[tabPossible$Freq > 0, ]
		tabPossible$Var1 <- as.numeric(levels(tabPossible$Var1))[tabPossible$Var1]
		notBetween <- is.na(x$BetweenGel) & !is.na(x$WithinGel)
		if(sum(notBetween) > 0){
			Candidate <- which(notBetween)
			if(length(Candidate) > 1){
				Candidate <- sample(Candidate, 1)
			}
			tabPossible <- tabPossible[tabPossible$Var1 == x$Plate[Candidate], ]
			if(nrow(tabPossible) > 1){
				whichGel <- sample(x = tabPossible$Var1, size = 1, prob = tabPossible$Freq)
			} else {
				whichGel <- x$Plate[Candidate]
			}
			x$BetweenGel[Candidate] <- whichGel
		} else {
			Replicateable <- is.na(x$WithinGel) & x$Plate %in% tabPossible$Var1
			Candidate <- sample(which(Replicateable), 1)
			x$WithinGel[Candidate] <- x$Plate[Candidate]
		}
	}
	#extra fill of the gels if needed. Only needed when the number of specimens is smaller than half the number of lanes.
	toFill <- nGels * (nLanes - nQC) - sum(!is.na(x[, 2:4]))
	if(toFill > 0){
		x <- rbind(x, data.frame(Specimen = sample(x$Specimen, toFill, replace = TRUE), Plate = sample(seq_len(nGels), toFill, replace = TRUE), WithinGel = NA, BetweenGel = NA))
	}
	#create a dataframe with the samples per well in the different microwell plates
	Result <- x[, c("Specimen", "Plate")]
	Result <- rbind(Result, na.omit(data.frame(Specimen = x$Specimen, Plate = x$WithinGel)))
	Result <- rbind(Result, na.omit(data.frame(Specimen = x$Specimen, Plate = x$BetweenGel)))
	#randomize the position of the samples within a plate
	Result <- Result[order(Result$Plate, runif(nrow(Result))), ]
	#label the positions
	Result$Lane <- rep(seq_len(nLanes - nQC), nGels)
	Result$Replicate <- factor(seq_len(nrow(Result)))
	if(nQC > 0){
		QC <- expand.grid(Specimen = seq_len(nQC), Plate = seq_len(nGels))
		QC$Lane <- -QC$Specimen + nLanes + 1
		QC$Replicate <- paste("QC", QC$Specimen, QC$Plate, sep = "_")
		QC$Specimen <- paste("QC", QC$Specimen)
		Result <- rbind(Result, QC)
	}
	if(!is.null(Group)){
		Result <- merge(Result, data.frame(Specimen = Specimens, Group = Group), all.x = TRUE)
	} else {
		Result$Group <- NA
	}
	Result <- Result[order(Result$Plate, Result$Lane), c("Plate", "Lane", "Replicate", "Specimen", "Group")]
	Result$Capilar <- factor("A")
	Result$Plate <- factor(Result$Plate)
	Result$Lane <- factor(Result$Lane)
	if(nQC > 0){
		return(
			new("AFLP", 
				Specimens = unique(Result[, c("Specimen", "Group")]),
				Replicates = Result[, c("Replicate", "Specimen", "Plate", "Capilar", "Lane")],
				QC = list(
					Specimen = data.frame(Specimen = unique(QC$Specimen), Type = "QC"),
					Replicate = data.frame(Replicate = QC$Replicate, Type = "QC")
				)
			)
		)
	} else {
		return(
			new("AFLP", 
				Specimens = unique(Result[, c("Specimen", "Group")]),
				Replicates = Result[, c("Replicate", "Specimen", "Plate", "Capilar", "Lane")]
			)
		)
	}
}
