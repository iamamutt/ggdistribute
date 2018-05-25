# Package info ------------------------------------------------------------

#' ggdistribute: `ggplot2` Extension for Plotting Distributions
#'
#' `ggdistribute` Uses the ggproto system to provide different geoms, stats, and
#' positions for plotting distributions.
#'
#' `ggdistribute` Displaying the distributions relies heavily on stacking
#' distributions using position_spread, which may not align with other geoms.
#'
#' @seealso [ggplot2::geom_density], [ggplot2::position_dodge]
#'
#' See `help(package = "ggdistribute")` for a list of functions.
#'
#' View vignettes with `browseVignettes(package ="ggdistribute")`.
#'
#' @import ggplot2
#' @import data.table
#' @importFrom magrittr "%>%"
#' @importFrom grDevices col2rgb gray rainbow rgb
#' @importFrom stats density ecdf mad median na.omit quantile rgamma rnbinom
#' rnorm runif sd
#' @importFrom utils capture.output modifyList
#' @docType package
#' @name ggdistribute-package
NULL



#' Geom for plotting posterior distributions
#'
#' @name GeomPosterior
#'
#' @inheritParams ggplot2::geom_violin
#' @param geom Use to override the default connection between `geom_posterior`
#' and `stat_density_ci`
#' @param draw_ci *geom*. Toggles drawing of the confidence interval lines and
#' segments.
#' @param draw_sd *geom*. Toggles drawing of the standard deviation interval
#' lines and segments.
#' @param midline_color *geom*. Color of the vertical, center line. Set to `NA`
#' to omit the line.
#' @param brighten *geom*. Numeric adjustments to the fill color. A value above
#' 1 increases brightness, below decreases. Should be of length 1 or 5,
#' otherwise values are recycled
#' @param interp_thresh *geom*. If the number of samples used to estimate the
#' density is low, this will result in gaps between segments. This argument
#' decides to interpolate points based on gap proportion for a segment.
#' @param mirror *geom*. Show standard densities (`mirror=FALSE`) or horizontal
#' violin densities (`mirror=TRUE`).
#' @param stat Used to override the default connection between `geom_posterior`
#' and `stat_density_ci`.
#'
#' @param center_stat *stat*. character string of method to compute the
#' distribution's central tendency, such as `"median"`, `"mean"`, or `"mode"`.
#' @param interval_type *stat*. method of computing the interval, either `"hdi"`
#' or `"ci"`
#' @param ci_width *stat*. Width of the distribution's confidence/highest
#' density interval, e.g., 0.95
#' @param trim If a value between 0 and 1 is given, trim the tails of `x` by
#' some proportion according to `trim`. If `NULL` or `NA`, don't trim the
#' tails. See [trim_ends()].
#' @param cut The values to use for the start and end of the density estimation
#' are `cut` bandwidths (e.g., `0.5*bw`) *beyond* the extremes of the data.
#' This allows the estimated density to drop to approximately zero at the
#' extremes.
#' @param bw The smoothing bandwidth to be used. If numeric, the standard
#' deviation of the smoothing kernel. If character, a rule to choose the
#' bandwidth, as listed in [stats::bandwidth]. If the bandwidth character
#' starts wit a `"."` (e.g., `".nrd0"`), then the average bandwidth will be
#' calculated among all groups in a panel and used for each density estimate.
#' @inheritParams ggplot2::stat_density
#' @section Aesthetics:
#' `geom_posterior` understands the following aesthetics (required
#' aesthetics are in bold):
#' - **x**
#' - **y**
#' - xmin
#' - xmax
#' - alpha
#' - colour
#' - fill
#' - group
#' - linetype
#' - size
#' - weight
#' @section Computed Variables:
#'
#' *stat_density_ci*:
#'
#' - density: density estimate from [stats::density]
#' - scaled: Normalized density values: `density / max(density)`
#' - count: Number of samples at density level: `dens: n`
#' - xmin: minimum value of `x` from the data
#' - cil: cil cutoff value based on `ci_width`
#' - sdl: central value minus 1 sd of `x`
#' - mid: value of central tendency
#' - sdu: central value plus 1 sd of `x`
#' - ciu: ciu cutoff value based on `ci_width`
#' - xmax: maximum value of `x` from the data
#' @examples
#' \dontrun{
#' x <- data_normal_sample(mu = c(-1, 0, 1), n = 500)
#'
#' p <- ggplot(x, aes(x = value))
#'
#' p + geom_posterior()
#'
#' p + geom_posterior(aes(y = Condition))
#'
#' p + geom_posterior(aes(y = GroupScore, fill = Condition))
#'
#' p + geom_posterior(aes(y = GroupScore, fill = Group),
#' brighten = c(1.3, 0, -1.3),
#' position = position_spread(
#' height=0.5,
#' padding = 0))
#'
#' }
#' @md
NULL

#' Print an example of the package functions
#'
#' @return NULL
#' @export
example_plot <- function() {
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
