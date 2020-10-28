library(epos)
context("test_readSecondLevelATC")

test_that("Test function readSecondLevelATC()", {
  atchashsec <-
    readSecondLevelATC(
    system.file("extdata", "atc-secondlevel.map", package = "epos"), "\t")
  expect_that(length(names(atchashsec)), equals(89))
})