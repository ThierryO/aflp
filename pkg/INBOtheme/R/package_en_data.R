#' @title Package with several ggplot2 themese
#' @description Lod this package AFTER loading ggplot2. theme_INBO() will be set as the default theme. 
#' @name INBOtheme-package
#' @aliases INBOtheme
#' @docType package
#' @author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}
#' @keywords package
#' @seealso \code{\link{theme_INBO}}, \code{\link{theme_elsevier}}
NULL

#'@name INBOblue
#'@aliases INBOblue INBObrown INBOdarkblue INBOdarkgreen INBOgreen INBOred INBOreddishbrown INBOextra
#'@title Colour according to the INBO style guide
#'@description Colour according to the INBO style guide as hexadecimal values.
#'\itemize{
#'  \item INBOblue
#'  \item INBOdarkblue
#'  \item INBOgreen
#'  \item INBOdarkgreen
#'  \item INBObrown
#'  \item INBOreddishbrown
#'  \item INBOred
#'  \item INBOextra
#'}
#'@docType data
#'@export
#'@usage INBOblue
#'@keywords datasets
#'@seealso \code{\link{theme_INBO}}
NULL

#'@name page.height
#'@aliases page.width, column.width, medium.width
#'@title Standard dimensions for theElsevier style guide
#'\itemize{
#'  \item{page.height}{Maximal height of a figuur (in inch)}
#'  \item{page.width}{With of a figure covering two columns (in inch)}
#'  \item{column.width}{With of a figure covering one column (in inch)}
#'  \item{medium.width}{With of a figure covering 1.5 columns (in inch)}
#'}
#'@docType data
#'@export
#'@usage page.height
#'@keywords datasets
#'@seealso \code{\link{theme_elsevier}}
NULL
