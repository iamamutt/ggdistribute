library(ggdistribute)
library(data.table)
context("Testing position spread")

test_that("Overlaps", {
  y_vals <- matrix(c(
    0, 1, 1, 2.5, 1.1, 2.5, 3, 4.9,
    4, 5, 4.7, 5.5, 5.49999, 5.5), nrow = 2)

  data <- data.table(
    group = as.integer(seq_len(ncol(y_vals))),
    PANEL = rep(1L, ncol(y_vals)))
  data <- data[
    , .(y = seq(y_vals[1L, group],
      y_vals[2L, group],
      length.out = 3)),
    .(PANEL, group)
  ]
  data[group == max(group), group := group + 2L]

  o <- ggdistribute:::get_overlaps(data, "y", tol = -1e-3)
  expect_identical(which(o$overlap), c(7L, 16L))
  expect_identical(which(o$within), c(7L, 21L))

  o <- ggdistribute:::get_overlaps(data[group == 9], "y", tol = -1e-3)
  expect_true(is.na(o$space))
})
