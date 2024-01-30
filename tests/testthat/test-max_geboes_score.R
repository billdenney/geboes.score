test_that("max_geboes_score", {
  expect_equal(max_geboes_score(c("4.1", "5.0"), all_categories = FALSE), "4.1")
  expect_equal(
    max_geboes_score(factor_geboes_score(c("4.1", "5.0")), all_categories = FALSE),
    factor_geboes_score("4.1")
  )
  expect_equal(
    max_geboes_score(c("0.0", "1.2", "2A.2", "2B.1", "3.3", "4.1", "5.0")),
    "4.1"
  )
  expect_equal(
    max_geboes_score(factor_geboes_score(c("0.0", "1.2", "2A.2", "2B.1", "3.3", "4.1", "5.0"))),
    factor_geboes_score("4.1")
  )
})
