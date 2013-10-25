#'The workhorse of the package.
#'
#'Takes a vector of zeros with length equal to a power of 2.
#'When the length is at least 2, this matrix is split in halve in to two equal
#'subvectors. Then the
#'\code{halvesRanking} function is applied recursively while increasing the level
#'with 1.
#'
#'When the \code{ranking} vector recudes to a scalar, the function returns
#'this scalar. When the \code{ranking} vector is split into two
#'subvectors, the numbers 0 and 1 are assigned at random to one of the
#'subvectors. Then the values of the subvector, after recursively applying the
#'\code{halvesRanking} function, is increased with the random number times 2 to the
#'power of the level.
#'
#'WARNING: This function does not do any checking on the sanity of \code{ranking} and
#'\code{level}. That would be a computational burden since the function is called
#'recursively. 
#'
#'@param ranking A vector with length equal to a power of 2.
#'Must start with a vector of zeros.
#'@param level the current levels. Defaults to 0.
#'@return A vector with the same dimension of \code{ranking} filled with a
#'randomised order of points.
#'@export

halvesRanking <- function(ranking, level = 0){
  if(length(ranking) > 1){
    sequence <- sample(0:1)
    v1 <- halvesRanking(
      head(ranking, length(ranking) / 2) + 
        sequence[1] * 2 ^ level, 
      level = level + 1
    )
    v2 <- halvesRanking(
      tail(ranking, length(ranking) / 2) + 
        sequence[2] * 2 ^ level, 
      level = level + 1
    )
    c(v1, v2)
  } else {
    ranking
  }
}
