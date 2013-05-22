#'Append fluorescence data from a SAGA file to an AFLP object
#'
#'Append fluorescence data from a SAGA file to an AFLP object.
#'
#'NOTE: the SAGA file must be in the default CSV format.
#'
#'
#'@param filename The SAGA file to be imported.
#'@param add.to The AFLP object were the fluorescence is appended to.
#'@param maxMissing A relative threshold. If more than this ratio of replicates
#'have missing data, then no data is appended.
#'@param textclean A user defined function to alter the names of the replicates
#'after reading the SAGA file.  Only useful when the names in the SAGA files do
#'not match with the names in the AFLP object. Directly using the correct names
#'is SAGA solves a lot of problems.
#'@return Return an AFLP object. If add.to is an AFLP object, then the
#'fluorescence data is appended to it. Otherwise a new AFLP object is created.
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{as.AFLP}}
#'@keywords manip
#'@examples
#'
#'  data(TiliaDesign)
#'  Tilia <- as.AFLP(TiliaDesign)
#'  Tilia <- readSAGA(
#'    system.file("extdata", "Tilia_bandvaluespc1", package = "AFLP"), 
#'    add.to = Tilia
#'  )
#'  Tilia <- readSAGA(
#'    system.file("extdata", "Tilia_bandvaluespc2", package = "AFLP"), 
#'    add.to = Tilia
#'  )
#'  Tilia <- readSAGA(
#'    system.file("extdata", "Tilia_bandvaluespc3", package = "AFLP"), 
#'    add.to = Tilia
#'  )
#'  Tilia <- readSAGA(
#'    system.file("extdata", "Tilia_bandvaluespc4", package = "AFLP"), 
#'    add.to = Tilia
#'  )
#'
#'@export
#'@importFrom reshape melt melt.data.frame
readSAGA <- function(filename, add.to, maxMissing = 0.25, textclean = function(x){x}){
  # #####################
  # Fooling R CMD check #
  #######################
  Replicate <- NULL
  # #####################
  # Fooling R CMD check #
  #######################
  
	Header <- which(substr(readLines(filename, warn = FALSE), 1, 80) == "Note:  Saga MX does not have a gel to gel image intensity calibration mechanism.")
	data <- read.csv(filename, sep = ",", dec = ".", skip = Header, stringsAsFactors = FALSE)
	data <- melt(data, id.vars = "Bins", variable_name = "Replication")
	data$value[data$value == 0] <- NA
	colnames(data) <- c("PC", "Replicate", "Fluorescence")
  if(class(data$PC) == "character"){
  	data$Marker <- as.numeric(substr(data$PC, nchar(data$PC) - 4, nchar(data$PC)))
  	data$PC <- factor(substr(data$PC, 2, nchar(data$PC) - 6))
  } else {
    data$Marker <- as.numeric(substr(levels(data$PC), nchar(levels(data$PC)) - 4, nchar(levels(data$PC))))[data$PC]
    data$PC <- factor(substr(levels(data$PC), 2, nchar(levels(data$PC)) - 6)[data$PC])
  }
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
