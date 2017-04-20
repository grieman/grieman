#' installnload_packages
#'
#' This function checks to see if packages are installed, installs them if needed, and then loads and attaches them. This can be useful in reproducable work, when one uses a more obscure package. Granted, containing it in a package creates another problem, but the code can be distributed and copied easily.
#'
#' @param packages a character vector of package names to be loaded
#'
#' @export
#'
installnload_packages <- function(packages){
  packages.new <- packages[!(packages %in% utils::installed.packages()[,"Package"])]
  if(length(packages.new)) utils::install.packages(packages.new)
  for (i in 1:length(packages)) library(packages[i], character.only = T)
}
