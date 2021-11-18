#' @export
vcov.data.frame <- function(object, ...) {
  attr(object, "var")
}
