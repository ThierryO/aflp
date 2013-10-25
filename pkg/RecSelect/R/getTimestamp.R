#' read all files in a directory and read their modification timestamp
#' @param path the path of the directory. Either an absolute path or relative to the working directory. If missing a dialog box will one where must choose a file within the directory
#' @param extension which file extension should be used? Defaults to 'WAV'. File extensions are not case sensitive.
#' @param recursive should all subdirectories be processed as well? Defaults to TRUE.
#' @return A \code{data.frame} with timestamp, path and file
#' @export
getTimestamp <- function(path, extension = "WAV", recursive = TRUE){
  if(missing(path)){
    path <- file.choose()
    path <- paste(
      head(
        strsplit(
          path, 
          "/"
        )[[1]], 
        -1
      ),
      collapse = "/"
    )
  }
  files <- list.files(
    path = path, 
    pattern = paste("\\.", extension, "$", sep = ""), 
    full.names = TRUE,
    recursive = recursive,
    ignore.case = TRUE
  )
  files <- file.info(files)[, "mtime", drop = FALSE]
  colnames(files) <- "timestamp"
  files[, c("path", "file")] <- t(sapply(
    strsplit(rownames(files), "/"),
    function(x){
      c(
        paste(
          head(x, -1),
          collapse = "/"
        ),
        tail(x, 1)
      )
    }
  ))
  rownames(files) <- NULL
  files
}
