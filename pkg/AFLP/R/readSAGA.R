readSAGA <- function(filename, add.to, maxMissing = 0.25, textclean = function(x){x}){
	Header <- which(substr(readLines(filename, warn = FALSE), 1, 80) == "Note:  Saga MX does not have a gel to gel image intensity calibration mechanism.")
	data <- read.csv(filename, sep = ",", dec = ".", skip = Header)
	data <- melt(data, id.vars = "Bins", variable_name = "Replication")
	data$value[data$value == 0] <- NA
	colnames(data) <- c("PC", "Replicate", "Fluorescence")
	data$Marker <- as.numeric(substr(data$PC, nchar(data$PC) - 4, nchar(data$PC)))
	data$PC <- factor(substr(data$PC, 2, nchar(data$PC) - 6))
	data$Normalised <- NA
	data$Score <- NA
	levels(data$Replicate) <- textclean(levels(data$Replicate))
	if(!all(levels(data$Replicate) %in% levels(replicates(add.to)$Replicate))){
		MissingReplicates <- !(levels(data$Replicate) %in% levels(replicates(add.to)$Replicate))
		if(mean(MissingReplicates) > maxMissing){
			stop("To much missing replicates. Data not added.")
		} else {
			warning("Not all replicates from the file exist in add.to. Missing replicates were omitted.")
			data <- subset(data, Replicate %in% levels(replicates(add.to)$Replicate))
		}
	}
	data$Replicate <- factor(data$Replicate, levels = levels(replicates(add.to)$Replicate))
	fluorescence(add.to) <- rbind(fluorescence(add.to), data)
	return(add.to)
}
