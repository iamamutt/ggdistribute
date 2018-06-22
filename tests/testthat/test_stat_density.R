library(ggdistribute)
context("Testing stat density computations")

set.seed(20130110)
x <- rnorm(1000, 100, 15)

test_that("List modification", {
  expect_named(compute_density(x), c(
    "x", "density", "scaled",
    "count", "n"
  ))

  defaults <- compute_conf_ints(x)

  expect_named(defaults, c("mid", "sdl", "sdu", "cil", "ciu"))
  expect_identical(defaults, compute_conf_ints(x, NA, NA, NA))
})
