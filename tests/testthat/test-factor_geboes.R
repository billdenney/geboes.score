test_that("levels_geboes_score", {
  expect_equal(
    levels_geboes_score(),
    as.character(geboes.score::geboes_grades$grade)
  )
  expect_equal(
    levels_geboes_score(drop_0 = TRUE),
    grep(x = geboes.score::geboes_grades$grade, pattern = "^[1-5][AB]?\\.0$", invert = TRUE, value = TRUE)
  )
})

test_that("factor_geboes_score", {
  expect_s3_class(factor_geboes_score("0.0"), "factor")
})
