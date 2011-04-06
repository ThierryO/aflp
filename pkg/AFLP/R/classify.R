classify <- function(data, output = c("screen", "none", "tex"), maxBorder = 1, tresholdPeakRatio = 0.03, tresholdMonomorph = 0, device = "pdf", path = NULL, nrows = 4, ncols = 4, keep.border = FALSE){
	output <- match.arg(output)
	if(output != "none"){
		if(!require(ggplot2)){
			stop("The ggplot2 package is required")
		}
		if(output == "tex"){
			if(!require(xtable)){
				stop("The xtable package is required")
			}
		}
	}
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
	Repeated2 <- cast(Specimen ~ ., data = Repeated, value = "Lane", fun = length)
	Repeated <- subset(Repeated, Specimen %in% Repeated2$Specimen[Repeated2[, 2] > 1])
	
	ExtraCols <- colnames(fluorescence(data))
	ExtraCols <- ExtraCols[!ExtraCols %in% c("PC", "Replicate", "Fluorescence", "Marker", "Normalised", "Score")]
	dataset <- fluorescence(data)
	dataset$Score <- NA
	xLimits <- range(dataset$Normalised)
	dataset$PCMarker <- paste(dataset$PC, dataset$Marker, sep = "_")
	#x <- subset(dataset, PCMarker == "PC4_680")
	#x <- subset(dataset, PCMarker == "B_100.803076923077")
	result <- dlply(dataset, "PCMarker", function(x){
		if(keep.border){
			Border <- border(data, x$PC, x$Marker)
		} else {
			Dens <- density(x$Normalised, n = 3000)
			Dens <- data.frame(Normalised = Dens$x, Density = Dens$y)
			Dens$First <- c(0, diff(Dens$Density))
			Dens$Second <- c(0, diff(Dens$First)) > 0
			Dens$Ratio <- Dens$Density / max(Dens$Density)
			Border <- Dens$Normalised[abs(Dens$First) < 1e-4]
			Dens$Peak[Dens$Normalised %in% Border & Dens$Second] <- "Valley"
			Dens$Peak[Dens$Normalised %in% Border & !Dens$Second & Dens$Ratio >= tresholdPeakRatio] <- "Peak"
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
			} else {
				x$Score <- ifelse(is.na(x$Sign) | x$Sign > 0, 1, 0)
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
				print(pDens + opts(title = paste("Density of normalised fluorescence for", unique(y$PC))))
			}
		})
		if(output == "tex"){
			cat("\\FloatBarrier\n\n")
		}
	})
	invisible(data)
}
