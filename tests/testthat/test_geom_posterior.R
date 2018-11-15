library(ggdistribute)
library(ggplot2)
context("Testing geom posterior")

test_that("Identity stat", {
  dt <- ggdistribute:::ggdist_data()
  d <- stats::density(dt$value, n=nrow(dt))
  dt$dx <- d$x
  dt$dy <- d$y
  dt$mid <- 4

  expect_s3_class(
    ggplot(dt, aes(x=dx, y=dy)) +
      geom_posterior(
        stat="identity", position="identity",
        draw_ci=FALSE, draw_sd=FALSE, aes(mid=mid)
      ) #
    , c("gg", "ggplot")
  )
})
