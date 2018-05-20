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

rebuild_showcase_plot <- F

## ---- echo=FALSE, message=FALSE------------------------------------------
library(ggplot2)
library(data.table)
library(magrittr)
theme_set(theme_gray(10))

## ------------------------------------------------------------------------
library(ggdistribute)

## ------------------------------------------------------------------------
mu <- c(-1, 0, 2, 5, 10)
value <- unlist(lapply(mu, function(x) rnorm(1000, x, runif(1, 0.8, 1.2))))
cond <- rep(letters[1:5], each = 1000)
grp <- rep(LETTERS[1:2], each = 2500)
         
data <- data.frame(grp, cond, value)

## ------------------------------------------------------------------------
b_cond <- data[with(data, cond == "b"), ]

ggplot(b_cond, aes(x = value))+
  geom_posterior()

## ------------------------------------------------------------------------
ggplot(data, aes(x = value, y = cond))+
  geom_posterior()

## ------------------------------------------------------------------------
ggplot(data, aes(x = value, y = cond))+
  geom_posterior(mirror = TRUE)+
  facet_grid(grp~., scales = "free_y")

## ------------------------------------------------------------------------
ggplot(data, aes(x = value, y = cond))+
  stat_density_ci(n = 1024, interp_thresh = .001)

## ------------------------------------------------------------------------
ggplot(data, aes(x = value, y = ..density.., fill=cond))+
  stat_density_ci(alpha = 0.5, n = 1024, geom = "density", position = "identity")

## ------------------------------------------------------------------------
ggplot(data, aes(x = value, y = cond, group = grp))+
  geom_posterior(position = position_spread(padding = 0))+
  theme(panel.grid.major.y = element_line(color = gray(.8)))

## ---- echo=FALSE, results='hide', message=FALSE--------------------------
if (rebuild_showcase_plot) {
  if (requireNamespace("mejr", quietly = TRUE)) {
    font <- "Oswald Medium"
    theme_set(mejr::theme_mejr(font_family = font))
  } else {
    font <- NULL
    theme_set(theme_gray())
  }
}

## ------------------------------------------------------------------------
theme_update(
  panel.grid.major.x = element_blank(),
  panel.ontop = FALSE, 
  panel.border = element_rect(fill = NA, 
                              colour = gray(0.84), 
                              size = 0.67),
  axis.title.y = element_blank(),
  strip.text.y = element_text(angle = 0, 
                              hjust = 0.5,
                              margin = margin(t = 0, r = 0, b = 0, l = 0, 
                                              unit = "pt")), 
  plot.margin = margin(t = 2, r = 4, b = 2, l = 2, unit = "pt"),
  legend.box.background = element_blank(),
  legend.background = element_blank()
)

colors <- list(
  black = "#141214",
  gray = "#939393",
  yellow = "#E0CF7C",
  magenta = "#E069C6",
  cyan = "#60ADAB")

## ------------------------------------------------------------------------
ex_plt <- 
  ggplot(sre_data(5000), aes(y = effect)) +
  facet_grid(contrast ~ ., scales = "free_y", space = "free_y") +
  labs(x = "Difference in accuracy (posterior predictions)") +
  geom_vline(
    color = colors$gray,
    size = 0.333,
    linetype = 1,
    xintercept = 0
  ) +
  geom_posterior(
    # geom specific aesthetics
    aes(x = value, fill = contrast),
    
    # position options
    position = position_spread(
      # order of groups within panels
      reverse = TRUE,
      # shrink heights of distributions
      padding = 0.3,
      # scale by heights within panels
      height = "panel"
    ),
    
    # geom options
    draw_ci = TRUE,
    draw_sd = TRUE,
    mirror = FALSE,
    midline_color = "#797979",
    brighten = c(1.6, 1, 1.2),
    interp_thresh = .001,
    
    # stat options for estimating intervals
    center_stat = "median",
    ci_width = 0.90,
    interval_type = "ci",
    
    # stat options passed to density estimation
    bw = ".nrd0",
    adjust = 1.5,
    n = 1024,
    trim = .005,
    cut = 1.5,
    
    # standard options
    size = 0.15,
    color = colors$gray,
    vjust = 0.7,
    show.legend = FALSE
  ) +
  scale_x_continuous(breaks = seq(-1, 1, .05)) +
  scale_fill_manual(values = c(colors$yellow, colors$magenta, colors$cyan))

## ---- fig.width=5, fig.height=3, out.width="100%"------------------------
plot(ex_plt)

## ---- echo=FALSE, message=FALSE, results='hide'--------------------------
if (requireNamespace("mejr", quietly = TRUE) && rebuild_showcase_plot) {
  mejr::save_plot(
    ex_plt,
    file = file.path("media", "example_fig"),
    format = "png",
    width = 5,
    height = 3,
    font = font,
    res = 600)
}

## ---- eval=FALSE, echo=FALSE---------------------------------------------
#  # other tests
#  dt <- ggdistribute:::ggdist_data(1000, j=5)
#  
#  ggplot(dt) + aes(x = value) +
#    geom_posterior(n=32)
#  
#  ggplot(dt) + aes(x = value) +
#    geom_posterior(n=32, aes(group=j_discrete))
#  
#  ggplot(dt) + aes(x = value, y=j_discrete) +
#    geom_posterior(n=32, position = position_spread(height = 20))
#  
#  ggplot(dt) + aes(x = value, y=j_discrete, group = k_discrete) +
#    geom_posterior(n=32)
#  
#  ggplot(dt) + aes(x = value, y=10) +
#    geom_posterior(n=32)
#  
#  ggplot(dt) + aes(x = value, y=j_discrete) +
#    geom_posterior(n=32, aes(group=j_discrete))
#  
#  ggplot(dt) + aes(x = value, y=j_discrete) +
#    geom_posterior(n=32, aes(fill=k_discrete))
#  
#  ggplot(dt) + aes(x = value, y=I) +
#    geom_posterior(n=32, aes(group=j_discrete))
#  
#  ggplot(dt) + aes(x = value, y=variable) +
#    geom_posterior(n=32, mirror = F, aes(group=j_discrete))
#  
#  ggplot(dt) + aes(x = value, y=variable * 10) +
#    geom_posterior(n=32, mirror = F, aes(group=j_discrete))+
#    facet_wrap(~k_discrete)
#  
#  ggplot(dt) + aes(x = value, y=k_discrete) +
#    geom_posterior(n=32, aes(group=j_discrete))

