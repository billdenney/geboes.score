test_that("max_geboes_score", {
  expect_equal(max_geboes_score(c("4.1", "5.0")), "4.1")
  expect_equal(
    max_geboes_score(factor_geboes_score(c("4.1", "5.0"))),
    factor_geboes_score("4.1")
  )
})
