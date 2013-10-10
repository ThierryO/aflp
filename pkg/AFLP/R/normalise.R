#'Perform a normalisation (calibration) procedure on the observed fluorescence.
#'
#'Calculates normalised fluorescence values by taking into account the
#'differences between replicates nested in specimens, plates and markers. This
#'is based on a linear mixed model. The form of the model is automatically
#'chosen based on the number of replicates, specimens, plates and marker. The
#'same form is apply on each primer combination separately.
#'
#'
#'@param data An AFLP object
#'@param output Which output is required. "screen" put QQ-plots of the random
#'effects, QQ-plots of the residuals and possible outliers on the screen. "tex"
#'givens the same information but saves the QQ-plots to files and report LaTeX
#'code to include the information in a LaTeX document.
#'@param path the path where the figures are saved. Only used if \code{output =
#'"tex"}.  Defaults to NULL, which is the working directory.
#'@param device the device to which the figures are saved. See
#'\code{\link[ggplot2]{ggsave}} for the available devices. Only used if
#'\code{output = "tex"}. Defaults to "pdf".
#'@param SpecimenEffect Add a random effect of the specimens to the model.
#'Defaults to FALSE.
#'@param level The level of the prediction intervals. Used to determine
#'possible outliers in the QQ-plots. Defaults to 0.99.
#'@param transformation Which transformation to use on the raw fluorescence
#'data. Valid choises are "log", "logit" and "none". Defaults to "log". "log"
#'implies the use of \code{log()}, hence no zero fluorescences are allowed. With
#'"logit", the raw fluorescence is first devided by the smallest power of 2,
#'which is still larger than the largest raw fluorescence. Then a logit
#'transformation is applied (\code{log(p/(1 - p))}).
#'@return
#'\itemize{
#'  \item data The altered data object that was put into the function.
#'Outliers in the \code{outliers} slot are removed from the \code{Fluorescence}
#'slot. The Normalised and Score columns from the \code{Fluorescence} slot are
#'overwritten.  Normalised holds the new normalised values. Score with be fill
#'with NA.
#' \item outliers An AFLP.outlier object with all possible outliers
#'detected by this procedure.
#'}
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{classify}}, \code{\link[ggplot2]{ggsave}}
#'@keywords models regression design
#'@export
#'@examples
#'
#'  data(Tilia)
#'  nOutput <- normalise(Tilia, output = "none")
#'
#'@importFrom plyr dlply . is.formula
#'@importFrom ggplot2 ggplot geom_line geom_ribbon geom_line aes aes_string
#'@importFrom xtable xtable print.xtable
#'@importFrom lme4 lmer ranef
normalise <- function(data, output = c("screen", "tex", "none"), path = NULL, device = "pdf", SpecimenEffect = FALSE, level = 0.99, transformation = c("log", "logit", "none")){
  # #####################
  # Fooling R CMD check #
  #######################
  fit <- lwr <- Observed <- PC <- Theoretical <- upr <- NULL
  # #####################
  # Fooling R CMD check #
  #######################

  output <- match.arg(output)
	transformation <- match.arg(transformation)
	if(output == "tex"){
    ThisRun <- paste(c("_", sample(c(letters, 0:9), 5)), collapse = "")
	}
	ExtraCols <- colnames(fluorescence(data))
  ExtraCols <- ExtraCols[!ExtraCols %in% c("PC", "Replicate", "Fluorescence", "Marker", "Normalised", "Score")]
	dataset <- merge(replicates(data), fluorescence(data))
  dataset$UseIt <- TRUE
	if(nrow(replicates(outliers(data))) > 0){
		dataset <- merge(dataset, cbind(replicates(outliers(data))[, c("PC", "Replicate")], remove = TRUE), all.x = TRUE)
    dataset$UseIt[!is.na(dataset$remove)] <- FALSE
		dataset$remove <- NULL
	}
	if(nrow(specimens(outliers(data))) > 0){
		dataset <- merge(dataset, cbind(specimens(outliers(data))[, c("PC", "Specimen")], remove = TRUE), all.x = TRUE)
		dataset$UseIt[!is.na(dataset$remove)] <- FALSE
		dataset$remove <- NULL
	}
	if(nrow(markers(outliers(data))) > 0){
		dataset <- merge(dataset, cbind(markers(outliers(data))[, c("PC", "Marker")], remove = TRUE), all.x = TRUE)
		dataset$UseIt[!is.na(dataset$remove)] <- FALSE
		dataset$remove <- NULL
	}
	if(nrow(residuals(outliers(data))) > 0){
		dataset <- merge(dataset, cbind(residuals(outliers(data))[, c("PC", "Replicate", "Marker")], remove = TRUE), all.x = TRUE)
		dataset$UseIt[!is.na(dataset$remove)] <- FALSE
		dataset$remove <- NULL
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
		formula <- paste(formula, "+ Capilar")
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
		} else if(nReplicate > 1){
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
  dataset$UseIt <- apply(!is.na(dataset[, c("Fluorescence", "PC", "Plate", "Lane", "Capilar", "Marker", "Replicate", "Specimen")]), 1, any) & dataset$UseIt
	#z <- subset(dataset, PC == "1")
	#z <- subset(dataset, PC == "PC1")
	results <- dlply(dataset, .(PC), function(z){
    currentPC <- z$PC[1]
		z$fMarker <- factor(z$fMarker)
		model <- lmer(data@model, data = z[z$UseIt, ])
		z$Normalised[z$UseIt] <- residuals(model)
		REF <- ranef(model)
		if("fMarker" %in% names(REF)){
			z$Sign <- REF[["fMarker"]][, "(Intercept)"][z$fMarker]
		}
		if(output == "tex"){
			cat("\\section{", levels(currentPC)[currentPC], "}\n\n", sep = "")
			PCn <- sub(":", "_", levels(currentPC)[currentPC])
			cat("\\begin{verbatim}\n")
			print(summary(model))
			cat("\\end{verbatim}\n")
		} else if (output == "screen"){
		  print(levels(currentPC)[currentPC])
		  print(summary(model))
		}
		Outliers <- lapply(names(REF), function(i){
			dfm <- data.frame(Observed = REF[[i]]$"(Intercept)", Label = rownames(REF[[i]]))
			dfm$Theoretical <- qnorm(ppoints(nrow(dfm)))[order(order(dfm$Observed))]
			dfm <- cbind(dfm, predict(lm(Observed ~ Theoretical, data = dfm), newdata = dfm, interval = "prediction", level = level))
			dfm$Outlier <- factor(ifelse(dfm$Observed >= dfm$lwr & dfm$Observed <= dfm$upr, "Acceptable", "Possible"), levels = c("Possible", "Acceptable"))
#m <- gam(Observed ~ s(Theoretical, bs = "cs", k = 3), data = dfm)
#dfm$fit <- fitted(m)        
			dfm$Outlier[with(dfm, Observed > min(Observed[Outlier == "Acceptable"]) & Observed < max(Observed[Outlier == "Acceptable"]))] <- "Acceptable"
			if(output != "none"){
				p <- 
				  ggplot(dfm, aes_string(x = "Theoretical", y = "Observed", ymin = "lwr", ymax = "upr")) + geom_ribbon(alpha = 0.1) + geom_line(aes_string(y = "fit")) + geom_point(aes_string(colour = "Outlier"))
			}
			Outlier <- dfm[dfm$Outlier != "Acceptable", c("Label", "Observed")]
			if(output == "tex"){
				cat("\\subsection{", i, "}\n\n", sep = "")
				caption <- paste("QQ-plot of the random effects at the level", i, "for primer combination", levels(currentPC)[currentPC])
				filename <- paste("RF", sub(":", "", i), PCn, ThisRun, ".", device, sep = "")
				ggsave.latex(p, caption = caption, filename = filename, width = 6, height = 4, path = path)
				if(nrow(Outlier) > 0){
					print(xtable(Outlier[order(Outlier$Observed), ], caption = caption, align = "llr", digits = 3, display = c("s", "s", "f")), include.rownames = FALSE, tabular.environment = "longtable", floating = FALSE, size = "tiny")
				}
				cat("\\FloatBarrier\n\n")
			} else if (output != "none"){
				dev.new()
				print(p + ggtitle(paste(levels(currentPC)[currentPC], i)))
				cat("\n\n", levels(currentPC)[currentPC], i, "\n")
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
    dfm <- z[z$UseIt, c("Normalised", "Replicate", "Marker")]
    colnames(dfm)[1] <- "Observed"
		dfm$Theoretical <- qnorm(ppoints(nrow(dfm)))[order(order(dfm$Observed))]
		dfm <- cbind(dfm, predict(lm(Observed ~ Theoretical, data = dfm), newdata = dfm, interval = "prediction", level = level))
		dfm$Outlier <- factor(ifelse(dfm$Observed >= dfm$lwr & dfm$Observed <= dfm$upr, "Acceptable", "Possible"), levels = c("Possible", "Acceptable"))
		dfm$Outlier[with(dfm, Observed > min(Observed[Outlier == "Acceptable"]) & Observed < max(Observed[Outlier == "Acceptable"]))] <- "Acceptable"
		if(output != "none"){
			p <- 
				ggplot(dfm, aes(x = Theoretical, y = Observed)) + geom_point(aes(colour = Outlier)) + geom_line(aes(y = fit)) + geom_line(aes(y = upr), linetype = 2) + geom_line(aes(y = lwr), linetype = 2)
		}
		Outlier <- dfm[dfm$Outlier != "Acceptable", c("Replicate", "Marker", "Observed")]
		if(output == "tex"){
			cat("\\subsection{Globale outliers}\n\n")
			caption <- paste("QQ-plot of the residuals for primer combination", levels(currentPC)[currentPC])
			filename <- paste("RFGlobal", PCn, ThisRun, ".", device, sep = "")
			ggsave.latex(p, caption = caption, filename = filename, width = 6, height = 4, path = path)
			print(xtable(Outlier[order(Outlier$Observed), ], caption = caption, align = "llrr", digits = 3, display = c("s", "s", "f", "f")), include.rownames = FALSE, tabular.environment = "longtable", floating = FALSE, size = "tiny")
			cat("\\FloatBarrier\n\n")
		} else if (output != "none"){
			dev.new()
			print(p + ggtitle(paste(levels(currentPC)[currentPC], "residuals")))
			cat("\n\n", levels(currentPC)[currentPC], "\n")
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
    z$UseIt <- NULL
		list(z = z, Outliers = AFLP.outlier(
			Replicate = oReplicate[with(oReplicate, order(PC, Observed)), ],
			Specimen = oSpecimen[with(oSpecimen, order(PC, Observed)), ],
			Marker = oMarker[with(oMarker, order(PC, Observed)), ],
			Residual = oResidual[with(oResidual, order(PC, Observed)), ])
		)
	})
  dataset <- do.call("rbind", lapply(results, function(z)z$z))
  Outliers <- do.call("rbind.AFLP.outlier", lapply(results, function(z)z$Outliers))
	if("Sign" %in% colnames(dataset)){
		ExtraCols <- c("Sign", ExtraCols)
	}
	data@Fluorescence <- dataset[, c("PC", "Replicate", "Fluorescence", "Marker", "Normalised", "Score", ExtraCols)]
	invisible(list(data = data, outliers = Outliers))
}
