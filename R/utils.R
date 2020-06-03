#' @export
vcov.data.frame <- function(x) {
  attr(x, "var")
}
