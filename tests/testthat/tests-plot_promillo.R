# library(testthat)
# library(tidyverse)
# source("promillo.R")

context("show_me_how_drunk")

test_that("expect object of class ggplot", {
    expect_class(
    show_me_how_drunk(
      age = 39,
      sex = "male",
      height = 190,
      weight = 87,
      drinking_time = as.POSIXct(c("2016-10-03 17:15:00", "2016-10-03 22:55:00")),
      drinks = c("massn" = 3, "schnaps" = 4)
    ),
    "ggplot")
})
test_that("expect correct output", {
  expect_equivalent(
    get_permille(
      alcohol_drunk = 40,
      bodywater = 32,
      drinking_time = as.POSIXct(c("2016-10-03 14:30:00", "2016-10-03 21:00:00"))
    ),
    tail(list_alcohol_course(
      alcohol_drunk = 40,
      bodywater = 32,
      drinking_time = as.POSIXct(c("2016-10-03 14:30:00", "2016-10-03 21:00:00")),
      distance = 5
      ), (n = 1)),
    tolerance = 0.01)
})
