#' create a random order for the timestamps
#' @param timestamp a vector of timestamps
#' @param min.difference the minimal difference between two timestamps in seconds.
#' @return a vector with the new order of the timestamps
#' @export

randomiseTimestamp <- function(timestamp, min.difference = 10){
  candidate <- seq(
    min(timestamp) - min.difference,
    max(timestamp) + min.difference,
    by = min.difference
  )
  id <- as.integer(cut(timestamp, breaks = candidate))
  rm(timestamp)
  n.candidate <- length(candidate)
  rm(candidate)
  ranking <- halvesRanking(
    ranking = rep(
      0,
      2 ^ ceiling(log2(n.candidate))
    )
  )
  ranking <- head(
    ranking,
    -floor(
      (length(ranking) - n.candidate) / 2
    )
  )
  ranking <- head(ranking, n.candidate)
  order(ranking[id])
}
