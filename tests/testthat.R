library(stringr)
library(testthat)
library(WinPerfCounter)

context("reading")
test_that("str_length is number of characters", {
  expect_equal(str_length("a"), 1)
  expect_equal(str_length("ab"), 2)
  expect_equal(str_length("abc"), 3)
  expect_equal(1+2, 3)
})

test_that("rounding_date", {
  a <- WinPerfCounter.readcsv("testthat/test.csv")
  b <- WinPerfCounter.alignTime(a, "15 minutes")
  expect_equal(b, as.POSIXct(
    c("2017-12-30 20:45:00 JST", "2017-12-30 21:00:00 JST")))
})
