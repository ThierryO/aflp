\name{getTimestamp}
\alias{getTimestamp}
\title{read all files in a directory and read their modification timestamp}
\usage{
  getTimestamp(path, extension = "WAV", recursive = TRUE)
}
\arguments{
  \item{path}{the path of the directory. Either an absolute
  path or relative to the working directory. If missing a
  dialog box will one where must choose a file within the
  directory}

  \item{extension}{which file extension should be used?
  Defaults to 'WAV'. File extensions are not case
  sensitive.}

  \item{recursive}{should all subdirectories be processed
  as well? Defaults to TRUE.}
}
\value{
  A \code{data.frame} with timestamp, path and file
}
\description{
  read all files in a directory and read their modification
  timestamp
}

