#' @export
print.dice_rolls <- function(x, ...) {
  cat("<dice_rolls> ", x$expr, "\n", sep = "")
  cat("Values: ", paste(x$values, collapse = ", "), "\n")
  invisible(x)
}
