#' Installs D3 and D3plus
#' @export
#' @return Copies a folder named \code{d3plus} to the working directory and it contains the \code{js} files and icons to make the visualizations
#' @importFrom utils unzip
#' @examples
#' install_d3plus()
#' @keywords functions

install_d3plus <- function() {
  file.copy(from=system.file("extdata", "d3plus.zip", package = "oec"), to=getwd())
  unzip("d3plus.zip")
  file.remove("d3plus.zip")
}
