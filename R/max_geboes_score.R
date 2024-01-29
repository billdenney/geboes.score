#' Determine the maximum value of the Geboes score
#'
#' Since the values ending in ".0" indicate no effect, they are not considered
#' in which values is the maximum.  In other words, "2.0" < "1.1".
#'
#' @inheritParams assert_geboes_score
#' @inheritParams base::max
#' @returns The maximum value of `x` where values ending in ".0" are considered
#'   as "0.0"
#' @export
max_geboes_score <- function(x, na.rm = TRUE) {
  assert_geboes_score(x)
  stopifnot(length(x) > 0)
  # This works with both character and factor variables since "0.0" is already a
  # level in geboes scores
  x[endsWith(as.character(x), ".0")] <- "0.0"
  max(x, na.rm = na.rm)
}
