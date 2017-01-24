#' Copies the demo file
#' @export
#' @return Copies a file named \code{demo_examples.R} to the working directory.
#' @examples
#' # demos()
#' @keywords functions

demos <- function() {
  file.copy(from=system.file("extdata", "demo_examples.R", package = "oec"), to=getwd())
}
