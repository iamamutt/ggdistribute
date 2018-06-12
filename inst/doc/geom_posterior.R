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
  out.width = "90%"
)

## ----load-and-opts, echo=FALSE, message=FALSE----------------------------
library(ggplot2)
library(data.table)
theme_set(theme_gray(10))

## ----load-ggdistribute---------------------------------------------------
library(ggdistribute)

## ----make-data-----------------------------------------------------------
data <- data_normal_sample(mu = c(-1, 0, 2, 5, 10), n = 1000)

## ----ex1-----------------------------------------------------------------
b_cond <- data[with(data, Condition == "B"), ]

ggplot(b_cond, aes(x = value))+
  geom_posterior()

## ----ex2-----------------------------------------------------------------
ggplot(data, aes(x = value, y = Condition))+
  geom_posterior()

## ----ex3, fig.height=6---------------------------------------------------
ggplot(data, aes(x = value, y = Condition))+
  geom_posterior(mirror = TRUE)+
  facet_grid(Group~., scales = "free_y")

## ----ex4-----------------------------------------------------------------
ggplot(data, aes(x = value, y = Condition)) +
  stat_density_ci(n = 1024, interp_thresh = .001)

## ----ex5-----------------------------------------------------------------
ggplot(data, aes(x = value, y = ..density.., fill = Condition)) +
  stat_density_ci(
    alpha = 0.5,
    n = 1024,
    geom = "density",
    position = "identity"
  )

## ----ex6-----------------------------------------------------------------
data$Group[data$Condition == "E"] <- "z"

ggplot(data, aes(x = value, y = Condition, group = Group)) +
  geom_posterior(position = position_spread(padding = 0)) +
  theme(panel.grid.major.y = element_line(color = gray(.8)))

## ----ex-plot-show, echo=FALSE, results='asis'----------------------------
ggdistribute:::function2chunk("example_plot")

## ----ex-plot-print, fig.width=5, fig.height=3, out.width="100%"----------
plot(example_plot())

## ----other-tests, eval=FALSE, echo=FALSE---------------------------------
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

