#' save_as_png
#'
#' @param plotme plot to be saved
#' @param file path at which plot will be saved
#' @param width im pixels
#' @param height in pixels
#' @param axes boolean that controls axes
#'
#' @return generates an image file at the designated path
#' @export
#'
#' @examples
save_as_png <- function(plotme, file, width=512, height=512, axes=T){
  grDevices::png(file, width, height)
  graphics::plot(plotme, axes=axes)
  grDevices::dev.off()
}
