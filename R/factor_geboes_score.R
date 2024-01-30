#' Convert a Geboes score into a factor
#'
#' @inheritParams assert_geboes_score
#' @return `x` as a factor, verified to be a Geboes score
#' @export
factor_geboes_score <- function(x, drop_0 = FALSE) {
  assert_geboes_score(x)
  if (drop_0) {
    x[endsWith(as.character(x), ".0")] <- "0.0"
  }
  factor(x, levels = levels_geboes_score(drop_0 = drop_0), ordered = TRUE)
}

#' @describeIn factor_geboes_score Get all available levels of the Geboes score
#' @param drop_0 Drop the ".0" levels other than "0.0"
#' @export
levels_geboes_score <- function(drop_0 = FALSE) {
  if (drop_0) {
    idx_nonzero <- -which(geboes.score::geboes_grades$grade_num == 0)[-1]
    ret <- geboes.score::geboes_grades$grade[idx_nonzero]
  } else {
    ret <- geboes.score::geboes_grades$grade
  }
  # It should return the levels, not a factor
  as.character(ret)
}
