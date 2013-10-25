\name{randomiseFiles}
\alias{randomiseFiles}
\title{Copy a set of files with a randomised order.}
\usage{
  randomiseFiles(path, output, recursive = TRUE,
    sub.dir = TRUE, extension = "WAV")
}
\arguments{
  \item{path}{the path of the directory with the files.
  Either an absolute path or relative to the working
  directory. If missing a dialog box will one where must
  choose a file within the directory.}

  \item{output}{the path of the directory were the new
  files should be placed. Either an absolute path or
  relative to the working directory. If missing a dialog
  box will one where must choose a file within the
  directory.}

  \item{extension}{which file extension should be used?
  Defaults to 'WAV'. File extensions are not case
  sensitive.}

  \item{recursive}{should all subdirectories be processed
  as well? Defaults to TRUE.}

  \item{sub.dir}{Process the files per subdirectory?
  Defaults to TRUE}
}
\description{
  Copy a set of files with a randomised order.
}

