# StatDensityCI wrapper ---------------------------------------------------

#' @describeIn GeomPosterior stat_density_ci Computes a distribution density and
#' confidence intervals for each group
#' @export
stat_density_ci <- function(mapping=NULL, data=NULL, geom="Posterior",
                            position="spread", ..., center_stat="median",
                            ci_width=0.9, interval_type="ci", bw="nrd0", adjust=1,
                            kernel="gaussian", cut=1, n=1024, trim=0.01,
                            na.rm=FALSE, show.legend=NA, inherit.aes=TRUE) {
  layer(
    data=data, mapping=mapping, stat=StatDensityCI, geom=geom,
    position=position, show.legend=show.legend, inherit.aes=inherit.aes,
    params=list(...,
      center_stat=center_stat, ci_width=ci_width,
      interval_type=interval_type, bw=bw, adjust=adjust,
      kernel=kernel, cut=cut, n=n, trim=trim, na.rm=na.rm)
  )
}


# ggproto object ----------------------------------------------------------

StatDensityCI <- ggproto(
  "StatDensityCI",
  Stat,
  required_aes="x",
  default_aes=aes(y=..count..),

  setup_params=function(data, params) {
    params$bw <- calc_avg_bw(data, params$bw)
    params
  },

  compute_panel=function(self, data, scales, ...) {
    if (scales$x$is_discrete()) {
      stop("in stat_density_ci, `x` cannot be discrete.")
    }

    as_dtbl(data) %>%
      split_discrete_by_group(scales, "y") %>%
      .[, self$compute_group(.SD, ...), .(PANEL, group)] %>%
      as.data.frame()
  },

  compute_group=function(data, center_stat="median", ci_width=0.9,
                           interval_type="ci", adjust=1, kernel="gaussian", cut=1,
                           n=1024, bw="nrd0", trim=0.01, na.rm=FALSE, ...) {
    if (length(unique(data$x)) < 2) {
      warning("x contains a single unique value. Dropping group.")
      return(NULL)
    }

    # density coords
    dens <- compute_density(
      x=data$x, y=data$y, w=NULL, cut=cut, bw=calc_bw(data$x, bw),
      adjust=adjust, kernel=kernel, n=n, trim=trim
    )

    if (all_missing(dens$x)) {
      return(NULL)
    }

    # confidence interval coords
    cint <- compute_conf_ints(data$x, center_stat, ci_width, interval_type)

    # combine with other data not used
    dist_data <- cbind(dens, cint)
    to_append <- get_static_data(data, dist_data)

    if (is.null(to_append)) {
      return(dist_data)
    }

    cbind(dist_data, to_append)
  }
)

# subfunctions ------------------------------------------------------------

compute_density <- function(x, y=NULL, w=NULL, cut=1, bw="nrd0", adjust=1,
                            kernel="gaussian", n=512, trim=NULL) {
  x <- is.null(trim) %?% x %:% trim_ends(x, trim, na.rm=TRUE)
  n_x <- length(x)

  missing_y <- all_missing(y)
  calc_options <- list(density=NA_real_, scaled=NA_real_, count=NA_real_)
  calc_list <- missing_y %?% calc_options %:% list(y=NA_real_)

  df <- c(x=NA_real_, calc_list, n=n_x)

  if (n_x < 2) {
    warning("Returning NA while trying to compute density. ",
      "`x` must be at least length 2, ", "or set `na.rm=TRUE`.",
      call.=FALSE)
    return(as.data.frame(df))
  }

  dcoord <- stats::density(
    x,
    weights=w, bw=bw, adjust=adjust, kernel=kernel, n=n, cut=cut
  )

  # actual number of samples
  n <- length(dcoord$y)

  # make sure zero density
  min_y <- which.min(dcoord$y)

  if (dcoord$y[min_y] > 0) {
    left <- min_y / (n / 2) < 1
    nearest_min_i <- seq(max(1, min_y - 1), min(n, min_y + 1))
    x_adj <- mean(diff(dcoord$x[nearest_min_i]))
    x_adj <- left %?% (-x_adj) %:% x_adj
    after <- left %?% (min_y - 1) %:% min_y
    n <- n + 1

    dcoord$x <- append(dcoord$x, dcoord$x[min_y] + x_adj, after)
    dcoord$y <- append(dcoord$y, 0, after)
  }

  # y will be taken from one of these calc cols if not in aes
  df$density <- dcoord$y
  df$scaled <- dcoord$y / max(dcoord$y)
  df$count <- norm_vec_sum(dcoord$y) * df$n

  # y is specified, position density at start of y
  if (!missing_y) {
    df$y <- min(y, na.rm=TRUE) + norm_vec_sum(dcoord$y)
  }

  df$x <- dcoord$x

  as.data.frame(df)
}

# x coords for cil ci, sd cil, mid, sd ciu, ciu ci
compute_conf_ints <- function(x, center_stat=NULL, ci_width=NULL, interval_type=NULL) {
  interval_frame <- data.frame(
    mid=NA_real_, sdl=NA_real_, sdu=NA_real_,
    cil=NA_real_, ciu=NA_real_
  )

  # setup defaults from missing
  ci_width <- max(ci_width %NA% 0.9)
  center_stat <- center_stat %NA% "median"
  interval_type <- interval_type %NA% "ci"

  # compute interval line positions regardless of being drawn
  markers <- post_int(x, mid=center_stat, int=interval_type, widths=ci_width)

  # assign to data which are to be drawn
  interval_frame$mid <- markers$c
  interval_frame$cil <- markers$l.wide
  interval_frame$ciu <- markers$r.wide
  interval_frame$sdl <- markers$l.sd
  interval_frame$sdu <- markers$r.sd

  interval_frame
}

# bandwidth calculator for density
calc_bw <- function(x, bw) {
  if (is.character(bw)) {
    if (length(x) < 2) {
      stop("need at least 2 points to select a bandwidth automatically", call.=FALSE)
    }
    bw <- switch(tolower(bw), nrd0=stats::bw.nrd0(x), nrd=stats::bw.nrd(x),
    ucv=stats::bw.ucv(x), bcv=stats::bw.bcv(x), sj= ,
    `sj-ste`=stats::bw.SJ(x, method="ste"),
    `sj-dpi`=stats::bw.SJ(x, method="dpi"), stop("unknown bandwidth rule"))
  }
  bw
}

# average bandwidth across groups
calc_avg_bw <- function(data, bw, grps="group") {
  # if NULL or NA then use individual bandwidth
  bw <- bw %NA% "nrd0"

  if (!grepl("^\\.", bw)) {
    return(bw)
  }

  bw_str <- sub(".", "", bw)

  as_dtbl(data) %>%
    .[!is.na(x), .(bw=calc_bw(x, bw_str)), by=grps] %>%
    .[, mean(bw, na.rm=TRUE)]
}
