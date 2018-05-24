## ----setup, include = FALSE, warning=FALSE, message=FALSE----------------
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>", strip.white = FALSE,
  tidy = FALSE, fig.align = "center", dpi = 300,
  fig.width = 5.25, fig.height = 3.8,
  out.width = "90%", out.height = "100%")

## ---- echo=FALSE, message=FALSE------------------------------------------
library(ggplot2)
library(data.table)
library(magrittr)
theme_set(theme_gray(10))
rebuild_showcase_plot <- TRUE

## ------------------------------------------------------------------------
library(ggdistribute)

## ------------------------------------------------------------------------
data <- data_normal_sample(mu = c(-1, 0, 2, 5, 10), n = 1000)

## ------------------------------------------------------------------------
b_cond <- data[with(data, cond == "B"), ]

ggplot(b_cond, aes(x = value)) + geom_posterior()

## ------------------------------------------------------------------------
ggplot(data, aes(x = value, y = cond)) + geom_posterior()

## ------------------------------------------------------------------------
ggplot(data, aes(x = value, y = cond)) + geom_posterior(mirror = TRUE) +
  facet_grid(grp ~ ., scales = "free_y")

## ------------------------------------------------------------------------
ggplot(data, aes(x = value, y = cond)) +
  stat_density_ci(n = 1024, interp_thresh = .001)

## ------------------------------------------------------------------------
ggplot(data, aes(x = value, y = ..density.., fill = cond)) +
  stat_density_ci(
    alpha = 0.5, n = 1024, geom = "density",
    position = "identity")

## ------------------------------------------------------------------------
ggplot(data, aes(x = value, y = cond, group = grp)) +
  geom_posterior(position = position_spread(padding = 0)) +
  theme(panel.grid.major.y = element_line(color = gray(.8)))

## ---- echo=FALSE, results='hide', message=FALSE--------------------------
font <- "serif"
if (rebuild_showcase_plot) {
  if (requireNamespace("extrafont", quietly = TRUE)) {
    library(extrafont)
    font <- "Oswald Medium"
  }
  theme_set(theme_mejr(base_family = font))
} else {
  theme_set(theme_gray(base_family = font))
}

## ---- echo=FALSE---------------------------------------------------------
ggdistribute:::print_fn(ex_plot_fn_chunk, example_plot)

## ----print_ex_plot, ref.label="ex_plot_fn_chunk"-------------------------
example_plot <- function() {
  colors <- mejr_palette()

  ggplot(sre_data(5000), aes(y = effect)) +
    facet_grid(contrast ~ ., scales = "free_y", space = "free_y") +
    labs(x = "Difference in accuracy (posterior predictions)") +
    geom_vline(
      color = colors$gray, size = 0.333,
      linetype = 1, xintercept = 0) +
    geom_posterior(
      # geom specific aesthetics
      aes(x = value, fill = contrast),

      # position options
      position = position_spread(
        # order of groups within panels
        reverse = TRUE, # shrink heights of distributions
        padding = 0.3, # scale by heights within panels
        height = "panel"), # geom options
      draw_ci = TRUE, draw_sd = TRUE, mirror = FALSE,
      midline_color = "#797979", brighten = c(3, 0, 1.5),
      interp_thresh = .001, # stat options for estimating intervals
      center_stat = "median", ci_width = 0.90,
      interval_type = "ci", # stat options passed to density estimation
      bw = ".nrd0", adjust = 1.5, n = 1024, trim = .005,
      cut = 1.5, # standard options
      size = 0.15, color = colors$gray, vjust = 0.7, show.legend = FALSE) +
    scale_x_continuous(breaks = seq(-1, 1, .05)) +
    scale_fill_manual(values = c(
      colors$yellow, colors$magenta,
      colors$cyan)) +
    theme(
      panel.grid.major.x = element_blank(), panel.ontop = FALSE,
      panel.border = element_rect(
        fill = NA, colour = gray(0.84), size = 0.67
      ),
      axis.title.y = element_blank(),
      strip.text.y = element_text(angle = 0, hjust = 0.5),
      plot.margin = margin(t = 2, r = 4, b = 2, l = 2, unit = "pt"),
      legend.box.background = element_blank(),
      legend.background = element_blank())
}

## ---- fig.width=5, fig.height=3, out.width="100%"------------------------
plot(example_plot())

## ---- echo=FALSE, message=FALSE, results='hide'--------------------------
if (rebuild_showcase_plot) {
  ggsave(
    file = file.path("media", "example_fig.png"),
    example_plot(), device = "png", width = 5,
    height = 3, units = "in", family = font, dpi = 600)
}

## ---- eval=FALSE, echo=FALSE---------------------------------------------
# # other visual inspections
# dt <- ggdistribute:::ggdist_data(1000, j = 5)
#
# ggplot(dt) + aes(x = value) +
# geom_posterior(n = 512, interp_thresh = .001) + labs(title = "no y")
#
# ggplot(dt) + aes(x = value, y = 10) +
# geom_posterior(n = 512, interp_thresh = .001) + labs(title = "scalar y")
#
# ggplot(dt) + aes(x = value, y = j_discrete) +
# geom_posterior(n = 512, interp_thresh = .001) + labs(title = "char y")
#
# ggplot(dt) + aes(x = value, y = j_discrete, group = k_discrete) +
# geom_posterior(n = 512, interp_thresh = .001) +
# labs(title = "char y, w/ group")
#
# ggplot(dt) + aes(x = value, y = k_discrete) +
# geom_posterior(n = 512, interp_thresh = .001, aes(group = j_discrete)) +
# labs(title = "char y, w/ group switched")
#
# ggplot(dt) + aes(x = value) +
# geom_posterior(n = 512, interp_thresh = .001, aes(group = j_discrete)) +
# labs(title = "no y, w/ group")
#
# ggplot(dt) + aes(x = value, y = j_discrete) +
# geom_posterior(n = 512, interp_thresh = .001, aes(group = j_discrete)) +
# labs(title = "char y, w/ same y group")
#
# ggplot(dt) + aes(x = value, y = j_discrete) +
# geom_posterior(n = 512, interp_thresh = .001, aes(fill = k_discrete)) +
# labs(title = "char y, w/ diff group")
#
# ggplot(dt) + aes(x = value, y = I) +
# geom_posterior(n = 512, interp_thresh = .001, aes(group = j_discrete)) +
# labs(title = "integer y, w/ group")
#
# ggplot(dt) + aes(x = value, y = j_discrete) +
# geom_posterior(
# n = 512, interp_thresh = .001,
# position = position_spread(height = 20)) +
# labs(title = "manual height, w/ group")
#
# ggplot(dt) + aes(x = value, y = variable) +
# geom_posterior(
# n = 512, interp_thresh = .001,
# mirror = TRUE, aes(group = j_discrete)) +
# labs(title = "cont. y, w/ group, mirrored")
#
# ggplot(dt) + aes(x = value, y = variable * 10) +
# geom_posterior(
# n = 512, interp_thresh = .001, mirror = TRUE,
# aes(group = j_discrete)) + facet_wrap(~k_discrete) +
# labs(title = "cont. y, w/ group, wrap, mirrored")
#
# dt$k[dt$j == 1 & dt$k == 2] <- NA
#
# ggplot(dt) + aes(x = value, y = j_discrete, group = k_discrete) +
# geom_posterior(n = 512, interp_thresh = .001) +
# facet_grid(j_discrete ~ ., scales = "free_y") +
# labs(title = "char y, w/ group, grid, missing group")
#
# ggplot(dt) + aes(x = value, group = k_discrete) +
# geom_posterior(n = 512, interp_thresh = .001) +
# facet_grid(j_discrete ~ ., scales = "free_y") +
# labs(title = "no y, w/ group, grid, missing group")
