\name{read.fsa.bins}
\alias{read.fsa.bins}
\title{Estimate optimal binning from fsa files}
\usage{
  read.fsa.bins(files, path = "", dye, SizeStandard,
    Range = range(SizeStandard), binwidth = 1, SNR = 20,
    verbose = TRUE)
}
\arguments{
  \item{files}{A vector of filenames. Can be a relative or
  an absolute path.}

  \item{path}{The base path for the filenames. Defaults to
  the working directory.}

  \item{dye}{A vector with the names of the dyes used for
  the data.}

  \item{SizeStandard}{A numeric vector with basepairs of
  the size standard.}

  \item{Range}{A numeric vector of length 2 with the lower
  and upper limit of the desired markers (in number of base
  pairs).}

  \item{binwidth}{The desired width of the bins.}

  \item{SNR}{Maximum signal-to-noise ratio for peaks of the
  size standard. Used to ignore unwanted very strong
  peaks.}

  \item{verbose}{Print the name of each file when
  processing it. Useful for tracking progress.}
}
\value{
  A names list with the breaks for each dye.
}
\description{
  Estimate optimal binning from fsa files.
}
\author{
  Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul
  Quataert
}
\seealso{
  \code{\link{read.fsa}}
}
\keyword{manip}

