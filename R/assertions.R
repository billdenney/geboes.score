#' Check if a value is a valid Geboes score
#'
#' @param x A character vector of Geboes score grades
#' @param all_categories Must `x` include all categories exactly once?
#' @return `x`, if the values are valid; an error otherwise
assert_geboes_score <- function(x, all_categories = FALSE) {
  if (is.factor(x)) {
    checkmate::assert_subset(x, choices = geboes.score::geboes_grades$grade)
  } else {
    checkmate::assert_subset(x, choices = as.character(geboes.score::geboes_grades$grade))
  }
  if (all_categories) {
    subscore_subcategories <- c("0", "1", "2A", "2B", "3", "4", "5")
    checkmate::assert_permutation(
      x = gsub(x = x, pattern = "\\..*$", replacement = ""),
      y = subscore_subcategories
    )
  }
  x
}
