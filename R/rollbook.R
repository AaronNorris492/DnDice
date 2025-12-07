#' Create an empty rollbook
#' @export
create_rollbook <- function() {
  structure(list(rolls = list()), class = "rollbook")
}

#' Add a named roll
#' @export
add_roll <- function(book, name, expr) {
  stopifnot(inherits(book, "rollbook"))
  stopifnot(is.character(name), length(name) == 1)
  stopifnot(is.character(expr), length(expr) == 1)

  parse_dice(expr)  # validates it

  book$rolls[[name]] <- expr
  book
}

#' Roll a named roll
#' @export
roll_named <- function(book, name, times = 1, seed = NULL) {
  stopifnot(inherits(book, "rollbook"))
  if (!(name %in% names(book$rolls))) stop("No such roll: ", name)
  roll_dice(book$rolls[[name]], times, seed)
}
