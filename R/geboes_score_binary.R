#' Convert a Geboes score from the original value to a normal/abnormal value
#'
#' Values <= "3.1" or ending with ".0" are considered "normal"; values > "3.1"
#' are considered abnormal based on Li 2019.
#'
#' @inheritParams assert_geboes_score
#' @return A character vector the same length as `x` with "normal" or "abnormal"
#' @references Li K, Strauss R, Marano C, et al. A Simplified Definition of
#'   Histologic Improvement in Ulcerative Colitis and its Association With
#'   Disease Outcomes up to 30 Weeks from Initiation of Therapy: Post Hoc
#'   Analysis of Three Clinical Trials. Journal of Crohn’s and Colitis.
#'   2019;13(8):1025-1035. doi:10.1093/ecco-jcc/jjz022
#' @examples
#' geboes_score_binary(c("3.1", "4.0", "3.2")) # c("Normal", "Normal", "Abnormal")
#' @export
geboes_score_binary <- function(x) {
  assert_geboes_score(x)
  geboes.score::geboes_grades$normal[as.integer(factor_geboes_score(x))]
}
