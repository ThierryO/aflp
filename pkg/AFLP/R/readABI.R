readABI <- function(filename, add.to){
	if(tail(strsplit(filename, "\\.")[[1]], 1) == "gz"){
		dataset <- read.delim(gzfile(filename))
	} else {
		dataset <- read.delim(filename)
	}
	fluorescence(add.to) <- 
		rbind(
			fluorescence(add.to),
			with(dataset, 
				subset(
					data.frame(
						PC = factor(Dye.Color), 
						Replicate = factor(Sample.File.Name), 
						Marker = Size, 
						Fluorescence = Height, 
						Normalised = NA, 
						Score = NA
					), 
					!is.na(Marker)
				)
			)
		)
	return(add.to)
}
