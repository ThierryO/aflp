read.fsa.bins <- function(files, path = "", dye, SizeStandard, Range = range(SizeStandard), binwidth = 1, SNR = 20, verbose = TRUE){
  require(seqinr, quietly = TRUE)
  require(signal, quietly = TRUE)
  require(zoo, quietly = TRUE)
  Peaks <- do.call(rbind, lapply(seq_along(files), function(i){
    filename <- files[i]
    if(verbose) cat(filename, "\n\r")
    #reading the data from the fsa file
    pattern <- read.abif(paste(path, filename, sep = ""))
    #only keeping the required information
    Selection <- c("DATA.1", "DATA.2", "DATA.3", "DATA.4")[c(pattern$Data$DyeN.1, pattern$Data$DyeN.2, pattern$Data$DyeN.3, pattern$Data$DyeN.4) %in% dye]
    pattern <- do.call(cbind, pattern$Data[c(Selection, "DATA.105")])
    pattern[, "DATA.105"] <- scale(pattern[, "DATA.105"])[, 1]
    #sizing of the data
    n <- 10 * length(SizeStandard)
    Peak <- cumsum(abs(c(0, diff(pattern[, "DATA.105"] > quantile(pattern[, "DATA.105"], 1 - n / nrow(pattern))))))
    Index <- seq_len(nrow(pattern))[Peak %% 2 == 1][pattern[Peak %% 2 == 1, "DATA.105"] == ave(pattern[Peak %% 2 == 1, "DATA.105"], Peak[Peak %% 2 == 1], FUN = max)]
    Index <- Index[pattern[Index, "DATA.105"] <= SNR]
    while(length(Index) > length(SizeStandard)){
      n <- n - 1
      Peak <- cumsum(abs(c(0, diff(pattern[, "DATA.105"] > quantile(pattern[, "DATA.105"], 1 - n / nrow(pattern))))))
      Index <- seq_len(nrow(pattern))[Peak %% 2 == 1][pattern[Peak %% 2 == 1, "DATA.105"] == ave(pattern[Peak %% 2 == 1, "DATA.105"], Peak[Peak %% 2 == 1], FUN = max)]
      Index <- Index[pattern[Index, "DATA.105"] <= SNR]
    }  
    while(length(Index) < length(SizeStandard)){
      n <- n + 1
      Peak <- cumsum(abs(c(0, diff(pattern[, "DATA.105"] > quantile(pattern[, "DATA.105"], 1 - n / nrow(pattern))))))
      Index <- seq_len(nrow(pattern))[Peak %% 2 == 1][pattern[Peak %% 2 == 1, "DATA.105"] == ave(pattern[Peak %% 2 == 1, "DATA.105"], Peak[Peak %% 2 == 1], FUN = max)]
      Index <- Index[pattern[Index, "DATA.105"] <= SNR]
    }
    if(length(Index) > length(SizeStandard)){
      if(length(Index) - length(SizeStandard) == 1){
        Index <- Index[-which.max(sapply(seq_along(Index), function(i){
          summary(lm(SizeStandard ~ stats::poly(Index[-i], 2)))$r.squared
        }))]
      } else {
        stop("More than one extra Index")
      }
    }
    model0 <- lm(SizeStandard ~ Index)
    Fs <- 1/coef(model0)[2]
    window <- 15
    fftn <- 2 ^ ceiling(log2(window))
    spg <- specgram(pattern[, "DATA.105"], fftn, Fs, window, window - binwidth)
    Peak <- scale(abs(spg$S[which.min(abs(spg$f - 1)), ]))[, 1]
    Index <- which(Peak == rollmax(Peak, k = 1 + 2 * floor((min(diff(SizeStandard)) * Fs - 1) / 2), fill = -Inf))
    Index <- Index[Peak[Index] <= SNR]
    Index <- sort(Index[tail(order(Peak[Index]), length(SizeStandard))])
    Time <- spg$t[Index]
    model <- lm(SizeStandard ~ Time)
    fluorescence <- do.call(rbind, lapply(seq_along(dye), function(j){
      spg <- specgram(pattern[, j], fftn, Fs, window, window - binwidth)
      Peak <- abs(spg$S[which.min(abs(spg$f - 1)), ])
      Index <- which(Peak == rollmax(Peak, k = 1 + 2 * floor(Fs * binwidth/ 4), fill = -Inf))
      cbind(j, predict(model, newdata = data.frame(Time = spg$t[Index])))
    }))
    return(
      fluorescence[fluorescence[, 2] >= Range[1] & fluorescence[, 2] <= Range[2], ]
    )
  }))
  Breaks <- lapply(seq_along(dye), function(j){
    bp <- Peaks[Peaks[, 1] == j, 2]
    dens <- density(bp, bw = binwidth/4, n = 100 * diff(range(bp)))
    c(Range[1], dens$x[dens$y == -rollmax(-dens$y, k = 3, fill = -Inf)], Range[2])
  })
  names(Breaks) <- dye
  return(Breaks)
}

read.fsa <- function(files, path = "", dye, SizeStandard, Breaks = NULL, Range = range(SizeStandard), binwidth = 1, SNR = 20, verbose = TRUE){
  require(seqinr, quietly = TRUE)
  require(signal, quietly = TRUE)
  require(zoo, quietly = TRUE)
  if(missing(Breaks)){
    if(verbose){
      cat("Estimating optimal binning thresholds\n\n")
    }
    Breaks <- read.fsa.bins(files = files, path = path, dye = dye, SizeStandard = SizeStandard, Range = Range, verbose = verbose, binwidth = binwidth, SNR = SNR)
  }
  if(verbose){
    cat("\n\n Measuring fluorescence\n\n")
  }
  Peaks <- do.call(rbind, lapply(seq_along(files), function(i){
    filename <- files[i]
    if(verbose) cat(filename, "\n\r")
    #reading the data from the fsa file
    pattern <- read.abif(paste(path, filename, sep = ""))
    #only keeping the required information
    Selection <- c("DATA.1", "DATA.2", "DATA.3", "DATA.4")[c(pattern$Data$DyeN.1, pattern$Data$DyeN.2, pattern$Data$DyeN.3, pattern$Data$DyeN.4) %in% dye]
    pattern <- do.call(cbind, pattern$Data[c(Selection, "DATA.105")])
    pattern[, "DATA.105"] <- scale(pattern[, "DATA.105"])[, 1]
    #sizing of the data
    n <- 10 * length(SizeStandard)
    Peak <- cumsum(abs(c(0, diff(pattern[, "DATA.105"] > quantile(pattern[, "DATA.105"], 1 - n / nrow(pattern))))))
    Index <- seq_len(nrow(pattern))[Peak %% 2 == 1][pattern[Peak %% 2 == 1, "DATA.105"] == ave(pattern[Peak %% 2 == 1, "DATA.105"], Peak[Peak %% 2 == 1], FUN = max)]
    Index <- Index[pattern[Index, "DATA.105"] <= SNR]
    while(length(Index) > length(SizeStandard)){
      n <- n - 1
      Peak <- cumsum(abs(c(0, diff(pattern[, "DATA.105"] > quantile(pattern[, "DATA.105"], 1 - n / nrow(pattern))))))
      Index <- seq_len(nrow(pattern))[Peak %% 2 == 1][pattern[Peak %% 2 == 1, "DATA.105"] == ave(pattern[Peak %% 2 == 1, "DATA.105"], Peak[Peak %% 2 == 1], FUN = max)]
      Index <- Index[pattern[Index, "DATA.105"] <= SNR]
    }  
    while(length(Index) < length(SizeStandard)){
      n <- n + 1
      Peak <- cumsum(abs(c(0, diff(pattern[, "DATA.105"] > quantile(pattern[, "DATA.105"], 1 - n / nrow(pattern))))))
      Index <- seq_len(nrow(pattern))[Peak %% 2 == 1][pattern[Peak %% 2 == 1, "DATA.105"] == ave(pattern[Peak %% 2 == 1, "DATA.105"], Peak[Peak %% 2 == 1], FUN = max)]
      Index <- Index[pattern[Index, "DATA.105"] <= SNR]
    }
    if(length(Index) > length(SizeStandard)){
      if(length(Index) - length(SizeStandard) == 1){
        Index <- Index[-which.max(sapply(seq_along(Index), function(i){
          summary(lm(SizeStandard ~ stats::poly(Index[-i], 2)))$r.squared
        }))]
      } else {
        stop("More than one extra Index")
      }
    }
    model0 <- lm(SizeStandard ~ Index)
    Fs <- 1/coef(model0)[2]
    window <- 15
    fftn <- 2 ^ ceiling(log2(window))
    spg <- specgram(pattern[, "DATA.105"], fftn, Fs, window, window - binwidth)
    Peak <- scale(abs(spg$S[which.min(abs(spg$f - 1)), ]))[, 1]
    Index <- which(Peak == rollmax(Peak, k = 1 + 2 * floor((min(diff(SizeStandard)) * Fs - 1) / 2), fill = -Inf))
    Index <- Index[Peak[Index] <= SNR]
    Index <- sort(Index[tail(order(Peak[Index]), length(SizeStandard))])
    Time <- spg$t[Index]
    model <- lm(SizeStandard ~ Time)
    do.call(rbind, lapply(seq_along(dye), function(j){
      spg <- specgram(pattern[, j], fftn, Fs, window, window - binwidth)
      Peak <- abs(spg$S[which.min(abs(spg$f - 1)), ])
      aggregate(list(Fluorescence = Peak), by = data.frame(Filename = filename, Dye = dye[j], Marker = cut(spg$t, Breaks[[j]])), FUN = max)
    }))
  }))
  MidPoints <- do.call(rbind, lapply(seq_along(Breaks), function(x){
    data.frame(Dye = names(Breaks)[x], Marker = cut(head(Breaks[[x]], -1) + diff(Breaks[[x]]) / 2, Breaks[[x]]), Midpoint = head(Breaks[[x]], -1) + diff(Breaks[[x]]) / 2, Binwidth = diff(Breaks[[x]]))
  }))
  Peaks$Marker <- with(Peaks, interaction(Dye, Marker))
  MidPoints$Marker <- with(MidPoints, interaction(Dye, Marker))
  MidPoints$Dye <- NULL
  Peaks <- merge(Peaks, MidPoints, all.x = TRUE)
  Peaks$Marker <- Peaks$Midpoint
  Peaks$Midpoint <- NULL
  Peaks[, c("Filename", "Dye", "Marker", "Binwidth", "Fluorescence")]
}
