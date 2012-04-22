normalise <- function(data, output = c("screen", "tex", "none"), path = NULL, device = "pdf", SpecimenEffect = FALSE, level = 0.99, transformation = c("log", "logit", "none")){
	output <- match.arg(output)
	transformation <- match.arg(transformation)
	if(output != "none"){
		if(!require(ggplot2)){
			stop("The ggplot2 package is required for the graphics")
		}
		if(output == "tex"){
			if(!require(xtable)){
				stop("The xtable package is required for LaTeX output")
			}
		}
	}
	ExtraCols <- colnames(fluorescence(data))
	ExtraCols <- ExtraCols[!ExtraCols %in% c("PC", "Replicate", "Fluorescence", "Marker", "Normalised", "Score")]
	dataset <- merge(replicates(data), fluorescence(data))
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
	
	dataset$Specimen <- factor(dataset$Specimen)
	dataset$Replicate <- factor(dataset$Replicate)
	dataset$Plate <- factor(dataset$Plate)
	dataset$Capilar <- factor(dataset$Capilar)
	dataset$Lane <- factor(dataset$Plate:factor(dataset$Lane))
	dataset$fMarker <- factor(dataset$Marker)
	dataset$PC <- factor(dataset$PC)
	dataset$Normalised <- NA
	dataset$Score <- NA
	if(transformation == "logit"){
		Power <- min(which(max(dataset$Fluorescence) ^ (1/seq_len(32)) <= 2))
		formula <- paste("qlogis(Fluorescence/2^", Power, ") ~ 1", sep = "")
	} else if (transformation == "log"){
		formula <- "log(Fluorescence) ~ 1"
	} else {
		formula <- "Fluorescence ~ 1"
	}
	nPlate <- min(colSums(table(dataset$Plate, dataset$PC) > 0))
	if(nPlate > 5){
		formula <- paste(formula, "+ (1|Plate)")
	} else if(nPlate > 1){
		formula <- paste(formula, "+ Plate")
	}
	if(length(levels(dataset$Capilar)) > 5){
		formula <- paste(formula, "+ (1|Capilar)")
		if(length(levels(dataset$Lane)) > 5){
			formula <- paste(formula, "+ (1|Lane)")
		} else if(length(levels(dataset$Lane)) > 1){
			formula <- paste(formula, "+ Lane")
		}
	} else if(length(levels(dataset$Capilar)) > 1){
		formula <- paste(formula, "+ Capilar + ")
		if(length(levels(dataset$Lane)) > 5){
			formula <- paste(formula, "+ (1|Lane)")
		} else if(length(levels(dataset$Lane)) > 1){
			formula <- paste(formula, "+ Lane")
		}
	}
	if(SpecimenEffect){
	  nSpecimen <- min(colSums(table(dataset$Specimen, dataset$PC) > 0))
    uni <- unique(dataset[, c("Specimen", "Replicate", "PC")])
	  Replication <- mean(table(uni$Specimen, uni$PC))
		if(Replication > 1.05){
			if(Replication * nSpecimen > 5){
				formula <- paste(formula, " + (1|Specimen/Replicate)")
			} else if(Replication * nSpecimen > 1){
				formula <- paste(formula, " + Specimen/Replicate")
			}
		} else {
			if(nSpecimen > 5){
				formula <- paste(formula, " + (1|Specimen)")
			} else if(nSpecimen > 1){
				formula <- paste(formula, " + Specimen")
			}
		}
	} else {
		nReplicate <- min(colSums(with(dataset, table(Replicate, PC)) > 0))
		if(nReplicate > 5){
			formula <- paste(formula, " + (1|Replicate)")
		} else if(nSpecimen > 1){
			formula <- paste(formula, " + Replicate")
		}
	}
	nMarker <- min(colSums(table(dataset$Marker, dataset$PC) > 0))
	if(nMarker > 5){
		formula <- paste(formula, " + Marker + (1|fMarker)")
	} else if(nMarker > 1){
		formula <- paste(formula, " + fMarker")
	}
	data@model <- as.formula(formula)
	#z <- subset(dataset, PC == "B")
	#z <- subset(dataset, PC == "PC1")
	results <- daply(dataset, "PC", function(z){
		currentPC <- z$PC[1]
		z$fMarker <- factor(z$fMarker)
		model <- lmer(data@model, data = z)
		z$Normalised <- residuals(model)
		REF <- ranef(model)
		if("fMarker" %in% names(REF)){
			z$Sign <- REF[["fMarker"]][, "(Intercept)"][z$fMarker]
		}
		if(output == "tex"){
			cat("\\section{", currentPC, "}\n\n", sep = "")
			PCn <- sub(":", "_", currentPC)
			cat("\\begin{verbatim}\n")
			print(summary(model))
			cat("\\end{verbatim}\n")
		}
		Outliers <- lapply(names(REF), function(i){
			df <- data.frame(Observed = REF[[i]]$"(Intercept)", Label = rownames(REF[[i]]))
			df$Theoretical <- qnorm(ppoints(nrow(df)))[order(order(df$Observed))]
			df <- cbind(df, predict(lm(Observed ~ Theoretical, data = df), newdata = df, interval = "prediction", level = level))
			df$Outlier <- factor(ifelse(df$Observed >= df$lwr & df$Observed <= df$upr, "Acceptable", "Possible"), levels = c("Possible", "Acceptable"))
			df$Outlier[with(df, Observed > min(Observed[Outlier == "Acceptable"]) & Observed < max(Observed[Outlier == "Acceptable"]))] <- "Acceptable"
			if(output != "none"){
				p <- 
					ggplot(df, aes_string(x = "Theoretical", y = "Observed", ymin = "lwr", ymax = "upr")) + geom_ribbon(alpha = 0.1) + geom_line(aes_string(y = "fit")) + geom_point(aes_string(colour = "Outlier"))
			}
			Outlier <- df[df$Outlier != "Acceptable", c("Label", "Observed")]
			if(output == "tex"){
				cat("\\subsection{", i, "}\n\n", sep = "")
				caption <- paste("QQ-plot of the random effects at the level", i, "for primer combination", currentPC)
				filename <- paste("RF", sub(":", "", i), PCn, ".", device, sep = "")
				ggsave.latex(p, caption = caption, filename = filename, width = 6, height = 4, path = path)
				if(nrow(Outlier) > 0){
					print(xtable(Outlier[order(Outlier$Observed), ], caption = caption, align = "llr", digits = 3, display = c("s", "s", "f")), include.rownames = FALSE, tabular.environment = "longtable", floating = FALSE, size = "tiny")
				}
				cat("\\FloatBarrier\n\n")
			} else if (output != "none"){
				X11()
				print(p + opts(title = paste(currentPC, i)))
				cat("\n\n", currentPC, i, "\n")
				if(nrow(Outlier) > 0){
					cat("Possible outliers:\n")
					print(Outlier)
				} else {
					cat("Probably no outliers\n")
				}
			}
			Outlier
		})
		names(Outliers) <- names(REF)
		df <- data.frame(Observed = z$Normalised, Replicate = z$Replicate, Marker = z$Marker)
		df$Theoretical <- qnorm(ppoints(nrow(df)))[order(order(df$Observed))]
		df <- cbind(df, predict(lm(Observed ~ Theoretical, data = df), newdata = df, interval = "prediction", level = level))
		df$Outlier <- factor(ifelse(df$Observed >= df$lwr & df$Observed <= df$upr, "Acceptable", "Possible"), levels = c("Possible", "Acceptable"))
		df$Outlier[with(df, Observed > min(Observed[Outlier == "Acceptable"]) & Observed < max(Observed[Outlier == "Acceptable"]))] <- "Acceptable"
		if(output != "none"){
			p <- 
				ggplot(df, aes(x = Theoretical, y = Observed)) + geom_point(aes(colour = Outlier)) + geom_line(aes(y = fit)) + geom_line(aes(y = upr), linetype = 2) + geom_line(aes(y = lwr), linetype = 2)
		}
		Outlier <- df[df$Outlier != "Acceptable", c("Replicate", "Marker", "Observed")]
		if(output == "tex"){
			cat("\\subsection{Globale outliers}\n\n")
			caption <- paste("QQ-plot of the residuals for primer combination", currentPC)
			filename <- paste("RFGlobal", PCn, ".", device, sep = "")
			ggsave.latex(p, caption = caption, filename = filename, width = 6, height = 4, path = path)
			print(xtable(Outlier[order(Outlier$Observed), ], caption = caption, align = "llrr", digits = 3, display = c("s", "s", "f", "f")), include.rownames = FALSE, tabular.environment = "longtable", floating = FALSE, size = "tiny")
			cat("\\FloatBarrier\n\n")
		} else if (output != "none"){
			X11()
			print(p + opts(title = paste(currentPC, "residuals")))
			cat("\n\n", currentPC, "\n")
			if(nrow(Outlier) > 0){
				cat("Possible outliers:\n")
				print(Outlier)
			} else {
				cat("Probably no outliers\n")
			}
		}
		if(!is.null(Outliers$Replicate) && nrow(Outliers$Replicate) > 0){
      if(class(Outliers$Replicate$Label) == "character"){ 
			  oReplicate <- data.frame(PC = currentPC, Replicate = sapply(strsplit(Outliers$Replicate$Label, ":"), function(x)x[1]), Observed = Outliers$Replicate$Observed)
      } else {
        oReplicate <- data.frame(PC = currentPC, Replicate = sapply(strsplit(levels(Outliers$Replicate$Label), ":"), function(x)x[1])[Outliers$Replicate$Label], Observed = Outliers$Replicate$Observed)
      }
		} else {
			oReplicate <- data.frame(PC = character(), Replicate = character(), Observed = numeric())
		}
		if(!is.null(Outliers$Specimen) && nrow(Outliers$Specimen) > 0){
			oSpecimen <- data.frame(PC = currentPC, Specimen = Outliers$Specimen$Label, Observed = Outliers$Specimen$Observed)
		} else {
			oSpecimen <- data.frame(PC = character(), Specimen = character(), Observed = numeric())
		}
		if(nrow(Outliers$fMarker) > 0){
			oMarker <- data.frame(PC = currentPC, Marker = as.numeric(Outliers$fMarker$Label), Observed = Outliers$fMarker$Observed)
		} else {
			oMarker <- data.frame(PC = character(), Marker = numeric(), Observed = numeric())
		}
		if(nrow(Outlier) > 0){
			oResidual <- cbind(PC = currentPC, Outlier[, c("Replicate", "Marker", "Observed")])
		} else {
			oResidual <- data.frame(PC = character(), Replicate = character(), Marker = numeric(), Observed = numeric())
		}
		list(z = z, Outliers = AFLP.outlier(
			Replicate = oReplicate[with(oReplicate, order(PC, Observed)), ],
			Specimen = oSpecimen[with(oSpecimen, order(PC, Observed)), ],
			Marker = oMarker[with(oMarker, order(PC, Observed)), ],
			Residual = oResidual[with(oResidual, order(PC, Observed)), ])
		)
	})
	dataset <- do.call("rbind", lapply(results, function(z)z$z))
	if("Sign" %in% colnames(dataset)){
		ExtraCols <- c("Sign", ExtraCols)
	}
	Outliers <- do.call("rbind.AFLP.outlier", lapply(results, function(z)z$Outliers))
	data@Fluorescence <- dataset[, c("PC", "Replicate", "Fluorescence", "Marker", "Normalised", "Score", ExtraCols)]
	invisible(list(data = data, outliers = Outliers))
}
