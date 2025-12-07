#' Parse a D&D style dice expression
#'
#' @param expr A string like "2d6+3" or "1d20"
#'
#' @return A list with components n, sides, modifier
#' @export
#'

parse_dice <- function(expr) {
  stopifnot(is.character(expr), length(expr) == 1)

  # remove whitespace
  expr_clean <- gsub("\\s+", "", expr)

  # regex for NdS+M or NdS-M or dS+M etc
  m <- regexec("^([0-9]*)d([0-9]+)([+-][0-9]+)?$", expr_clean)
  parts <- regmatches(expr_clean, m)[[1]]

  if (length(parts) == 0) stop("Invalid dice expression: ", expr)

  n <- ifelse(parts[2] == "", 1L, as.integer(parts[2]))
  sides <- as.integer(parts[3])
  modifier <- ifelse(is.na(parts[4]), 0L, as.integer(parts[4]))

  list(
    n = n,
    sides = sides,
    modifier = modifier,
    expr = expr
  )
}
