## ----setup, include = FALSE, warning=FALSE, message=FALSE----------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  strip.white = FALSE,
  tidy = FALSE,
  fig.align = "center",
  dpi = 300,
  fig.width = 5.25,
  fig.height = 3.8,
  out.width = "90%",
  out.height = "100%"
)

## ---- echo=FALSE, message=FALSE------------------------------------------
library(ggplot2)
library(data.table)
theme_set(theme_gray(10))

## ------------------------------------------------------------------------
library(ggdistribute)

## ------------------------------------------------------------------------
data <- data_normal_sample(mu = c(-1, 0, 2, 5, 10), n = 1000)

## ------------------------------------------------------------------------
b_cond <- data[with(data, Condition == "B"), ]

ggplot(b_cond, aes(x = value))+
  geom_posterior()

## ------------------------------------------------------------------------
ggplot(data, aes(x = value, y = Condition))+
  geom_posterior()

## ------------------------------------------------------------------------
ggplot(data, aes(x = value, y = Condition))+
  geom_posterior(mirror = TRUE)+
  facet_grid(Group~., scales = "free_y")

## ------------------------------------------------------------------------
ggplot(data, aes(x = value, y = Condition)) +
  stat_density_ci(n = 1024, interp_thresh = .001)

## ------------------------------------------------------------------------
ggplot(data, aes(x = value, y = ..density.., fill = Condition)) +
  stat_density_ci(
    alpha = 0.5,
    n = 1024,
    geom = "density",
    position = "identity"
  )

## ------------------------------------------------------------------------
data$Group[data$Condition == "E"] <- "z"

ggplot(data, aes(x = value, y = Condition, group = Group)) +
  geom_posterior(position = position_spread(padding = 0)) +
  theme(panel.grid.major.y = element_line(color = gray(.8)))

## ---- echo=FALSE---------------------------------------------------------
ggdistribute:::print_fn(ex_plot_fn_chunk, example_plot)

## ----print_ex_plot, ref.label="ex_plot_fn_chunk"-------------------------
example_plot <-
function() {
  colors <- mejr_palette()

  ggplot(sre_data(5000), aes_string(y = "effect")) +
    facet_grid("contrast ~ .", scales = "free_y", space = "free_y") +
    labs(x = "Difference in accuracy (posterior predictions)") +
    geom_vline(
      color = colors$gray, size = 0.333,
      linetype = 1, xintercept = 0) +
    geom_posterior(
      # ------------------------
      # geom specific aesthetics
      # ------------------------
      aes_string(x = "value", fill = "contrast"),
      # ----------------
      # position options
      # ----------------
      position = position_spread(
        reverse = TRUE, # order of groups within panels
        padding = 0.3, # shrink heights of distributions
        height = "panel" # scale by heights within panels
      ), # ------------
      # geom options
      # ------------
      draw_ci = TRUE, # confidence interval parts
      draw_sd = TRUE, # standard deviation parts
      mirror = FALSE, # violion-like toggle
      midline_color = NULL, # color of line showing center of dist (uses color)
      brighten = c(3, 0, 1.333), # modify interval fill segments
      # -------------------------------------
      # stat options for estimating intervals
      # -------------------------------------
      interp_thresh = .001, # threshold for interpolating segment gaps
      center_stat = "median", # measure of central tendency
      ci_width = 0.90, # width corresponding to CI segments
      interval_type = "ci", # quantile intervals not highest density interval
      # -------------------------------------
      # stat options for density estimation
      # -------------------------------------
      bw = ".nrd0", # bandwidth estimator type
      adjust = 1.5, # adjustment to bandwidth
      n = 1024, # number of samples in final density
      trim = .005, # trim x before estimating density
      cut = 1.5, # tail extension
      # ----------------
      # standard options
      # ---------------
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

## ---- eval=FALSE, echo=FALSE---------------------------------------------
#  library(ggplot2)
#  
#  # other visual inspections
#  dt <- ggdistribute:::ggdist_data(1000, j = 5)
#  
#  ggplot(dt) + aes(x = value) +
#    geom_posterior(n = 512, interp_thresh = .001) + labs(title = "no y")
#  
#  ggplot(dt) + aes(x = value, y = 10) +
#    geom_posterior(n = 512, interp_thresh = .001) + labs(title = "scalar y")
#  
#  ggplot(dt) + aes(x = value, y = j_discrete) +
#    geom_posterior(n = 512, interp_thresh = .001) + labs(title = "char y")
#  
#  ggplot(dt) + aes(x = value, y = j_discrete, group = k_discrete) +
#    geom_posterior(n = 512, interp_thresh = .001) +
#    labs(title = "char y, w/ group")
#  
#  ggplot(dt) + aes(x = value, y = k_discrete) +
#    geom_posterior(n = 512, interp_thresh = .001, aes(group = j_discrete)) +
#    labs(title = "char y, w/ group switched")
#  
#  ggplot(dt) + aes(x = value) +
#    geom_posterior(n = 512, interp_thresh = .001, aes(group = j_discrete)) +
#    labs(title = "no y, w/ group")
#  
#  ggplot(dt) + aes(x = value, y = j_discrete) +
#    geom_posterior(n = 512, interp_thresh = .001, aes(group = j_discrete)) +
#    labs(title = "char y, w/ same y group")
#  
#  ggplot(dt) + aes(x = value, y = j_discrete) +
#    geom_posterior(n = 512, interp_thresh = .001, aes(fill = k_discrete)) +
#    labs(title = "char y, w/ diff group")
#  
#  ggplot(dt) + aes(x = value, y = I) +
#    geom_posterior(n = 512, interp_thresh = .001, aes(group = j_discrete)) +
#    labs(title = "integer y, w/ group")
#  
#  ggplot(dt) + aes(x = value, y = j_discrete) +
#    geom_posterior(
#      n = 512, interp_thresh = .001,
#      position = position_spread(height = 20)) +
#    labs(title = "manual height, w/ group")
#  
#  ggplot(dt) + aes(x = value, y = variable) +
#    geom_posterior(
#      n = 512, interp_thresh = .001,
#      mirror = TRUE, aes(group = j_discrete)) +
#    labs(title = "cont. y, w/ group, mirrored")
#  
#  ggplot(dt) + aes(x = value, y = variable * 10) +
#    geom_posterior(
#      n = 512, interp_thresh = .001, mirror = TRUE,
#      aes(group = j_discrete)) + facet_wrap(~k_discrete) +
#    labs(title = "cont. y, w/ group, wrap, mirrored")
#  
#  dt$k[dt$j == 1 & dt$k == 2] <- NA
#  
#  ggplot(dt) + aes(x = value, y = j_discrete, group = k_discrete) +
#    geom_posterior(n = 512, interp_thresh = .001) +
#    facet_grid(j_discrete ~ ., scales = "free_y") +
#    labs(title = "char y, w/ group, grid, missing group")
#  
#  ggplot(dt) + aes(x = value, group = k_discrete) +
#    geom_posterior(n = 512, interp_thresh = .001) +
#    facet_grid(j_discrete ~ ., scales = "free_y") +
#    labs(title = "no y, w/ group, grid, missing group")

