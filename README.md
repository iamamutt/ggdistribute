
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggdistribute

A `ggplot2` Extension for Plotting Distributions

## Example

The following example can be generated with the function
`example_plot()`.

``` r
library(ggplot2)
library(ggdistribute)
```

<img src="media/turtle_snails-1.png" width="100%" height="100%" style="display: block; margin: auto;" />

``` r
example_plot <-
function() {
  colors <- mejr_palette()

  ggplot(sre_data(5000), aes(y = effect)) +
    facet_grid(contrast ~ ., scales = "free_y", space = "free_y") +
    labs(x = "Difference in accuracy (posterior predictions)") +
    geom_vline(
      color = colors$gray, size = 0.333,
      linetype = 1, xintercept = 0) +
    geom_posterior(
      # ------------------------
      # geom specific aesthetics
      # ------------------------
      aes(x = value, fill = contrast),
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
      midline_color = "#797979", # color of line showing center of dist.
      brighten = c(3, 0, 1.5), # modify interval fill segments
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
```

Generate a sample dataset of Normal distributions.

``` r
dt <- data_normal_sample(mu = c(-1, 2, 3, 5), n = 2500)
```

``` r
# create new grouping variable from `grp`
dt$GRP <- with(dt, factor(grp,
  levels = letters[1:8],
  labels = c(
    rep("Low", 3), rep("Mid", 2),
    rep("High", 3)), ordered = TRUE))
```

Unique groups per `grp`, `cond`, and `GRP`.

``` r
unique(dt[, c("grp", "cond")])
#> # A tibble: 8 x 2
#>   grp   cond 
#>   <chr> <chr>
#> 1 a     A    
#> 2 b     A    
#> 3 c     B    
#> 4 d     B    
#> 5 e     C    
#> 6 f     C    
#> 7 g     D    
#> 8 h     D
```

### Facetting and spreading groups

``` r
ggplot(dt) + 
  aes(x = value, y = cond, group = grp) +
  geom_posterior(
    aes(fill = GRP),
    mirror = TRUE, 
    show.legend = FALSE,
    adjust = 1.5, 
    position = position_spread(reverse = TRUE)) +
  geom_point(
    aes(color = GRP, shape = cond),
    alpha = .08,
    fill = NA, 
    show.legend = FALSE, 
    position = position_jitter(0, .45)) +
  coord_cartesian(
    ylim = c(0.5, 2.5), 
    expand = FALSE) +
  facet_wrap(
    ~GRP, 
    scales = "free") +
  labs(
    title = "Example plot 2", 
    y = "Condition", 
    x = "Parameter estimate")
```

<img src="media/space_ships-1.png" width="80%" height="100%" style="display: block; margin: auto;" />

### Changing the appearance of `geom_posterior`

``` r
ggplot(dt) + 
  aes(x = value, y = grp) + 
  geom_vline(
    xintercept = 0, 
    size = .6) +
  geom_posterior(
    aes(color = cond),
    midline_color = NULL, 
    mirror = TRUE, 
    fill = "white",
    draw_sd = FALSE, 
    interval_type = "hdi",
    vjust = 0, 
    position = position_spread(height = 2)) +
  labs(
    title = "Example plot 3",
    x = "Parameter estimate", 
    y = "Sample location") +
  theme(
    legend.position = c(.025, .9), 
    legend.justification = c(0, 0),
    panel.grid.major.y = element_line(color = gray(.92)))
```

<img src="media/candy_wrappers-1.png" width="80%" height="100%" style="display: block; margin: auto;" />

## How to install

### Dependencies

  - R: <https://www.r-project.org/>

A current R installation.

#### Dependencies for installing the development version of this package

  - `devtools` package:
    <https://www.rstudio.com/products/rpackages/devtools/>

So that you can install the GitHub contents as an R package. You can
install the package by opening up RStudio or an R terminal and running:

``` r
install.packages("devtools")
```

  - Build tools: <http://cran.r-project.org/bin/windows/Rtools/>

For Windows users, you *may* be required to install Rtools first before
you can use the `devtools` package, if there is any code that needs to
be compiled. These are a set of build tools customized for building R
packages (see `devtools` link for more
details).

### Installing from CRAN

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/ggdistribute)](https://cran.r-project.org/package=ggdistribute)

If you want to use the last version that was uploaded to the CRAN
repository, do the following:

``` r
install.packages("ggdistribute")
```

### Installing from the downloaded package content folder

If you have all the package contents, you can open up the RScience.Rproj
file in RStudio and use both `devtools` and RStudio to load or install
package contents. First make sure you have all the package dependencies
to be able to load or install the `ggdistribute` package contents.

``` r
devtools::install_dev_deps()
```

Then build and install the package directory.

<!--
devtools::build(pkg = ".", path = "../tarballs", binary = FALSE, args = c("--md5"))
-->

``` r
devtools::install()
```

### Installing from GitHub

If `devtools` are installed, you may use the `install_github()` function
to download and install the development version of the package from this
GitHub repository instead of the one hosted on CRAN. Run the code below
to download and install the development version:

``` r
devtools::install_github("iamamutt/ggdistribute")
```

or to install all suggested packages as well…

``` r
devtools::install_github("iamamutt/ggdistribute", dependencies=TRUE)
```

If successful, the package should now be installed. Load the package as
you normally would any other package (see below). Repeat these steps if
there are updates to the package, or to reinstall on another machine You
should now be able to use the package materials and should see it in
your packages tab if using RStudio.

``` r
library(ggdistribute)
```

## Getting help

### Browsing the vignettes

Vignettes can be viewed in several different ways.

  - pre-built and saved in the `inst\doc` folder on GitHub.
  - calling `vignette("geom_posterior", "ggdistribute")` from within R
    after the package is installed.
  - navigating to packages tab \> ggdistribute \> User guides, package
    vignettes… in RStudio.

### Viewing the help documentation

View the package welcome page to navigate to different types of help
documents

``` r
package?ggdistribute
```

Viewing package information and a list of exported objects:

``` r
help(package = "ggdistribute")

# or
library(help="ggdistribute")
```
