#' Copies demo file with examples.
#' @export
#' @return A file named \code{demo_examples.R} will be copied to the working directory.
#' @examples
#' demos()
#' @keywords functions

demos <- function() {
  file.copy(from=system.file("extdata", "demo_examples.R", package = "oec"), to=getwd())
}
