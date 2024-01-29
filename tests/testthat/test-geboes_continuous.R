test_that("geboes_continuous", {
  expect_equal(
    geboes_continuous(c("0.0", "1.0", "2A.0", "2B.0", "3.0", "4.0", "5.0")),
    rep(0L, 7)
  )
  expect_equal(
    geboes_continuous(c("0.3", "1.3", "2A.3", "2B.3", "3.3", "4.3", "5.4")),
    c(3L, 6L, 9L, 12L, 15L, 18L, 22L)
  )
  # Invalid, not character or factor
  expect_error(
    geboes_continuous(c(0.3, 1.3, 2.3, 2.3, 3.3, 4.3, 5.6))
  )
  # Invalid 5.6 value
  expect_error(
    geboes_continuous(c("0.3", "1.3", "2A.3", "2B.3", "3.3", "4.3", "5.6")),
    regexp = "5\\.6"
  )
  # Duplicated 0. category
  expect_equal(
    geboes_continuous(c("0.3", "0.3", "2A.3", "2B.3", "3.3", "4.3", "5.4")),
    c(3L, 3L, 9L, 12L, 15L, 18L, 22L)
  )
})
