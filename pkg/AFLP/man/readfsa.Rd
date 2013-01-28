\name{read.fsa}
\Rdversion{1.1}
\alias{read.fsa}
\title{
Get fluorescence data from fsa files
}
\description{
Read fluorescence data from fsa files. }
\usage{
read.fsa(path = ".", files = NULL, dye, SizeStandard, 
  Breaks = NULL, Range = range(SizeStandard), 
  binwidth = 1, SNR = 20, verbose = TRUE)
}
\arguments{
  \item{files}{
	A vector of filenames. Can be a relative or an absolute path.
}
  \item{path}{
	The base path for the filenames. Defaults to the working directory.
}
  \item{dye}{
	A vector with the names of the dyes used for the data.
}
  \item{SizeStandard}{
	A numeric vector with basepairs of the size standard. 
}
  \item{Breaks}{
  A named list with the breaks used for binning the fluorescence. If missing the breaks are estimated by read.fsa.bins() 
}
  \item{Range}{
  A numeric vector of length 2 with the lower and upper limit of the desired markers (in number of base pairs). 
}
  \item{binwidth}{
  The desired width of the bins. 
}
  \item{SNR}{
  Maximum signal-to-noise ratio for peaks of the size standard. Used to ignore unwanted very strong peaks. 
}
  \item{verbose}{
  Print the name of each file when processing it. Useful for tracking progress.
}
}
\value{
	A data.frame with 5 variables: Filename, Dye, Marker, Binwidth and Fluorescence.
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}
\seealso{
\code{\link{read.fsa.bins}}
}
\keyword{manip}
