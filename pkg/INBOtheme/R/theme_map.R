#' a ggplot2 theme which removes labels, ticks and titles from both axes.
#' 
#'@export
#'@importFrom ggplot2 theme element_blank
theme_map <- function () {
  theme(
    axis.text = element_blank(), 
    axis.line = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    complete = FALSE)
}
