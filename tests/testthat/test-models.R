library(testthat)

test_that("Trees work", {
  expect_error(build.tree(1))
  expect_error(build.tree(as.Date("2020-01-01")))
  expect_error(build.tree(1))
  expect_error(build.tree(1))
  expect_error(build.tree(1))
})
