#' Determine the maximum value of the Geboes score
#'
#' Since the values ending in ".0" indicate no effect, they are not considered
#' in which values is the maximum.  In other words, "2.0" < "1.1".
#'
#' Typical use of `max_geboes_score()` is to find the maximum for a single
#' person at a single time.  For that, the `all_categories` argument ensures
#' that each category is present exactly once.
#'
#' @inheritParams assert_geboes_score
#' @inheritParams base::max
#' @returns The maximum value of `x` where values ending in ".0" are considered
#'   as "0.0"
#' @examples
#' max_geboes_score(c("0.0", "1.2", "2A.2", "2B.1", "3.3", "4.1", "5.0")) # 4.1
#' @export
max_geboes_score <- function(x, all_categories = TRUE, na.rm = TRUE) {
  assert_geboes_score(x, all_categories = all_categories)
  stopifnot(length(x) > 0)
  # This works with both character and factor variables since "0.0" is already a
  # level in geboes scores
  x[endsWith(as.character(x), ".0")] <- "0.0"
  max(x, na.rm = na.rm)
}
