\name{quality}
\alias{quality}
\title{Extract the quality data from an AFLP object}
\arguments{
  \item{x}{An AFLP object}

  \item{which}{Which quality information to extract. Must
  be one of c("all", "marker", "specimen", "replicate",
  "plate", "overall"). Defaults to "all".}
}
\value{
  In case when which == "all" a list with 3 data.frame
  containing the quality information.  Otherwise a
  data.frame with the selected quality data.
}
\description{
  Extracts the quality from an AFLP object
}
\details{
  Extracts the quality data of an AFLP object

  Extracts the quality of an AFLP object
}
\author{
  Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul
  Quataert
}

