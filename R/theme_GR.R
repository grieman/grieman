#' theme_GR
#'
#' @return the ggplot2 function theme_manual(), with chosen changes
#' @export
#'
#' @examples
#' d <- ggplot2::qplot(carat, data = ggplot2::diamonds[ggplot2::diamonds$color %in%LETTERS[4:7], ],
#'  geom = "histogram", bins=30, fill = color)
#' d + theme_GR()
#'
theme_GR <- function() {
  ggplot2::theme(axis.line = ggplot2::element_line(linetype = "solid"),
                                   panel.grid.major = ggplot2::element_line(colour = "gray80"),
                                   panel.grid.minor = ggplot2::element_line(colour = "gray90",
                                   linetype = "dashed"), panel.background = ggplot2::element_rect(fill = NA),
                                   legend.direction = 'horizontal', legend.position = 'bottom')
}
