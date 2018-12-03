library(stringr)
library(tibble)
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
  a <- WinPerfCounter.readcsv("test.csv")
  b <- WinPerfCounter.alignTime(a, "15 minutes")
  expect_equal(b, as.POSIXct(
    c("2017-12-30 20:45:00 JST", "2017-12-30 21:00:00 JST")))
})

test_that("huge_value", {
  a <- WinPerfCounter.readcsv("huge_value.csv")
  b <- WinPerfCounter.alignTime(a, "15 minutes")
  expect_equal(b, as.POSIXct(
    c("2017-12-30 20:45:00 JST", "2017-12-30 21:00:00 JST")))
})

test_that("read_csv", {
  a <- WinPerfCounter.readcsv("testthat/huge_value.csv")
})


test_that("read.vmmap", {
  filename <- "rstudio.csv"
  header <- VMMap.read.header(filename)
  expect_equal( header[header$Type == "Total", "Size"], tibble(Size = as.integer(c(866884))) )
  expect_equal( header[header$Type == "Page_Table", "Private_WS"], tibble(Private_WS = as.integer(c(2564))))
  body <- VMMap.read.body(filename)
  expect_equal( body[body$Address=="00020000" & body$ParentOrChild != "Parent", "Committed"], tibble(Committed = as.integer(c(64))))
  expect_equal( body[body$Address=="227E0000" & body$ParentOrChild == "Child", "Size"], tibble(Size = as.integer(c(9796))))
})

