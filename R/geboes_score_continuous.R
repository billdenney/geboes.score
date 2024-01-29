#' Convert maximum Geboes scores to a continuous measurement
#'
#' This function returns 0 for any grade ending in ".0" fitting with the intent
#' of the score as anything other than "0.0" cannot be a maximum.
#'
#' @inheritParams assert_geboes_score
#' @returns The numeric value of the continuous Geboes score (the integer value
#'   of the maximum grade)
#'
#' @references
#' See Supplementary Table 1 in the reference below
#'
#' Magro F, Lopes J, Borralho P, et al. Comparing the Continuous Geboes Score
#' With the Robarts Histopathology Index: Definitions of Histological Remission
#' and Response and their Relation to Faecal Calprotectin Levels. Journal of
#' Crohn’s and Colitis. 2020;14(2):169-175. doi:10.1093/ecco-jcc/jjz123
#'
#' @examples
#' # Generate all continuous scores
#' geboes_continuous(geboes.score::geboes_grades$grade)
#'
#' @export
geboes_continuous <- function(x) {
  assert_geboes_score(x)
  as.integer(factor_geboes_score(x, drop_0 = TRUE)) - 1
}
