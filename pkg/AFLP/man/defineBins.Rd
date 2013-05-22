\name{defineBins}
\alias{defineBins}
\title{Define bins in AFLP object}
\usage{
  defineBins(dataset, minPeakHeight, minBinWidth = 1,
    maxBinWidth = 5, missingPeakRatio = 0.8)
}
\arguments{
  \item{dataset}{An AFLP object}

  \item{minPeakHeight}{If set, only peaks with at least
  this amount of RFU will be taken into account when
  defining bins.}

  \item{minBinWidth}{Bins wider that this minimum AND who
  have at least one replicate with more than one peak in
  that bin, will be split into to smaller bins.}

  \item{maxBinWidth}{If a bin is wider than this number of
  basepairs, then the algorithm will split the bin,
  regardless the number of peaks within each replicate.}

  \item{missingPeakRatio}{If no peak is found for a give
  bin and given replicate, then a peak value will be given
  that is equal to mssingPeakRatio times the lowest peak
  value in the dataset.}
}
\value{
  A new AFLP object were the fluorescence slot is replaced
  by the bins.
}
\description{
  This function is only relevant when the AFLP object
  contains raw data of peak values.
}
\details{
  The peaks are aggregated into bins according to the
  algoritme described by Arrigo et al (2009)
  \cite{Arrigo2009}.
}
\author{
  Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul
  Quataert
}
\references{
  Arrigo, N., Tuszynski, J., Ehrich, D., Gerdes, T. and
  Alvarez, N. (2009), Evaluating the impact of scoring
  parameters on the structure of intra-specific genetic
  variation using RawGeno, an R package for automating AFLP
  scoring.  BMC Bioinformatics, 10:33,
  10.1186/1471-2105-10-33,
  \url{http://www.biomedcentral.com/1471-2105/10/33}
}
\seealso{
  \code{\link{normalise}}, \code{\link{readABI}}
}
\keyword{manip}

