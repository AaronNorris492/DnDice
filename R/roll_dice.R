#' Roll a dice expression one or more times
#'
#' @param expr A dice expression like "2d6+3"
#' @param times Number of times to roll
#' @param seed Optional seed for reproducibility. If NULL, a random seed is generated.
#'
#' @return A dice_rolls object containing results, the expression, parsed values, and the seed used.
#' @export

roll_dice <- function(expr, times = 1, seed = NULL) {
  parsed <- parse_dice(expr)

  if (is.null(seed)) {
    seed <- sample.int(.Machine$integer.max, 1)
  }

  set.seed(seed)

  rolls <- replicate(times, {
    sum(sample(parsed$sides, parsed$n, replace = TRUE)) + parsed$modifier
  })

  structure(
    list(
      values = rolls,
      expr = expr,
      parsed = parsed,
      seed = seed
    ),
    class = "dice_rolls"
  )
}
