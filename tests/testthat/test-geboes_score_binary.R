test_that("geboes_binary", {
  expect_equal(
    geboes_score_binary(geboes.score::geboes_grades$grade),
    geboes.score::geboes_grades$normal
  )
  expect_equal(
    geboes_score_binary(rev(geboes.score::geboes_grades$grade)),
    rev(geboes.score::geboes_grades$normal)
  )
  expect_equal(
    geboes_score_binary(c("3.1", "3.2", "4.0", "4.1")),
    c("Normal", "Abnormal", "Normal", "Abnormal")
  )
})
