#' Convert Geboes scores to a continuous measurement
#'
#' @inheritParams assert_geboes_score
#' @returns The numeric value of the continuous Geboes score (the sum of each of
#'   the category/subcategory scores)
#'
#' @references
#' Zenlea T, Yee EU, Rosenberg L, et al. Histology Grade Is Independently
#' Associated With Relapse Risk in Patients With Ulcerative Colitis in Clinical
#' Remission: A Prospective Study. Official journal of the American College of
#' Gastroenterology | ACG. 2016;111(5):685. doi:10.1038/ajg.2016.50
#' @export
geboes_continuous <- function(x) {
  assert_geboes_score(x, all_categories = TRUE)
  sum(as.integer(gsub(x = x, pattern = "^.*\\.", replacement = "")))
}
