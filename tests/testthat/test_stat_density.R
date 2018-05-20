library(ggdistribute)
context("Testing stat density computations")

set.seed(20130110)
x <- rnorm(1000, 100, 15)

test_that("has correct variables", {
  expect_named(ggdistribute:::compute_density(x), c(
    "x", "density", "scaled",
    "count", "n"))

  expect_named(
    ggdistribute:::compute_conf_ints(x),
    c("mid", "sdl", "sdu", "cil", "ciu"))

  expect_true(!all(unlist(lapply(ggdistribute:::compute_conf_ints(
    x, NA, NA, NA
  ), is.na))))
})
