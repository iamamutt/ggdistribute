# distribution functions --------------------------------------------------


#' Posterior intervals
#'
#' Returns cutoff points from a posterior distribution
#' @return data.table
#'
#' @param x Vector of numeric values. Typically a posterior sample.
#' @param mid Central tendency estimator. Defaults to `"median"`. Other options
#' include `"mean"` and `"mode"`.
#' @param widths interval widths
#' @param adj Bandwidth adjustment used only with the `"mode"` estimator. See
#' [dmode].
#' @param rope Region of practical equivalence. Check how much of the
#' distribution is within rope value.
#' @param warn Turn off warning for flat intervals found (multiple possible
#' values)
#' @param int interval type, either "hdi" or "ci"
#' @export
#' @examples
#' x <- rpois(5000, 15)
#' ints <- post_int(x, warn = FALSE)
#' hist(x, br=50)
#' abline(v=ints$c, col="cyan")
#' abline(v=ints[, c("l.wide", "r.wide")], col="magenta")
#'
#' post_int(x, "median", warn = FALSE)
#' post_int(x, "mean", warn = FALSE)
#' post_int(x, "mode", adj=2, rope = c(14, 16), warn = FALSE)
post_int <- function(x, mid=c("median", "mean", "mode"), int=c("hdi", "ci"),
                     widths=c(.50, .95), adj=1.5, rope=NULL, warn=FALSE) {
  if (!is.vector(x)) {
    stop("`x` must be a vector.")
  }

  mid <- match.arg(mid)
  int <- match.arg(int)

  center <- NA_real_
  scale <- NA_real_

  # measure of central tendency
  switch(mid,
    "mean"={
      center <- mean(x)
      scale <- sd(x)
    },
    "median"={
      center <- median(x)
      scale <- mad(x)
    },
    "mode"={
      center <- dmode(x, adjust=adj)
      scale <- mad(x)
    },
    NULL
  )

  # interval function as an unevaluated function call
  int_fun <- switch(
    int,
    "ci"={
      match.call(quantile, call("quantile",
        x=quote(x),
        probs=quote(c((1 - w) / 2, w + ((1 - w) / 2))),
        names=FALSE))
    },
    "hdi"={
      match.call(hdi, call("hdi", x=quote(x), prob=quote(w), warn=warn))
    },
    NULL
  )

  # mass within 1 sd range
  area_sd <- prop_in_range(x, center - scale, center + scale)

  # width identifiers
  widths <- sort(widths)
  width_ids <- sprintf("%.0f", widths * 100)
  width_ids[which.max(widths)] <- "wide"

  # left/right values for each width
  intervals <- data.table::rbindlist(
    Map(
      function(w, i) {
        ints <- structure(Reduce(cbind, eval(int_fun)),
          dimnames=list(NULL, c("l", "r")))
        data.table::data.table(interval=i, ints)
      },
      c(widths, area_sd),
      c(width_ids, "sd")
    )
  )

  central <- interval <- NULL

  # reshape to multivariate format
  intervals <- intervals[order(-interval), ]
  intervals <- intervals[, central := mid]
  intervals <- data.table::dcast(
    intervals, central ~ interval,
    value.var=c("l", "r"), sep="."
  )
  intervals[, c := center]
  set_col_order(intervals, c("central", "c", "l.sd", "l.wide", "r.sd", "r.wide"))

  # get rope
  if (!is.null(rope)) {
    if (is.character(rope) && rope == "max") {
      rope <- c(intervals$l.wide, intervals$r.wide)
    }
    if (length(rope) != 2) {
      stop("ROPE must be a minimum and maximum value")
    }
    rope <- sort(rope)
    rope_mass <- prop_in_range(x, rope[1], rope[2])
    intervals[, `:=`(l.rope=rope[1], r.rope=rope[2], rope=rope_mass)]
  }

  as.data.frame(intervals)
}

#' Highest density interval
#'
#' This is a function that will calculate the highest density interval from a
#' posterior sample.
#'
#' The default is to calcualte the highest 95 percent interval. It can be used
#' with any numeric vector instead of having to use one of the specific MCMC
#' classes. This function has been adapted from John K. Kruschke (2011). Doing
#' Bayesian Data Analaysis: A Tutorial with R and BUGS.
#'
#' @param x Numeric vector of a distribution of data, typically a posterior
#' sample
#' @param prob Width of the interval from some distribution. Defaults to `0.95`.
#' @param warn Option to turn off multiple sample warning message Must be in the
#' range of `[0,1]`.
#' @return Numeric range
#' @export
#' @examples
#' x <- qnorm(seq(1e-04, .9999, length.out=1001))
#' hdi_95 <- hdi(x, .95)
#' hdi_50 <- hdi(x, .50)
#'
#' hist(x, br=50)
#' abline(v=hdi_95, col="red")
#' abline(v=hdi_50, col="green")
#'
#' x <- exp(seq(pi * (1 - (1/16)), pi, len = 1000))
#' x <- c(x, rev(x)[-1])
#' x <- c(-x, x)
#' plot(sort(x), type="l")
#' plot(density(x, adjust=0.25))
#' abline(v=hdi(x, p=.49), col=2)
#' abline(v=hdi(x, p=.50), col=3)
hdi <- function(x, prob=0.95, warn=TRUE) {
  if (anyNA(x)) {
    stop("HDI: ", "x must not contain any NA values.", call.=FALSE)
  }

  N <- length(x)

  if (N < 3) {
    if (warn) {
      warning("HDI: ", "length of `x` < 3.", " Returning NAs", call.=FALSE)
    }
    return(c(NA_integer_, NA_integer_))
  }

  x_sort <- sort(x)
  window_size <- as.integer(floor(prob * length(x_sort)))

  if (window_size < 2) {
    if (warn) {
      warning("HDI: ", "window_size < 2.", " `prob` is too small or x does not ",
        "contain enough data points.", " Returning NAs.",
        call.=FALSE)
    }
    return(c(NA_integer_, NA_integer_))
  }

  lower <- seq_len(N - window_size)
  upper <- window_size + lower

  # vectorized difference between edges of cumulative distribution based on
  # scan_length. Values are arranged from left to right scanning.
  window_width_diff <- x_sort[upper] - x_sort[lower]

  # find minimum of width differences, check for multiple minima
  min_i <- which(window_width_diff == min(window_width_diff))
  n_candies <- length(min_i)

  if (n_candies > 1) {
    if (any(diff(sort(min_i)) != 1)) {
      if (warn) {
        warning("HDI: ", "Identical densities found along ",
          "different segments of the distribution.",
          " Choosing rightmost.",
          call.=FALSE)
      }
      min_i <- max(min_i)
    } else {
      min_i <- floor(mean(min_i))
    }
  }

  # get values based on minimum
  c(x_sort[min_i], x_sort[upper[min_i]])
}

#' Mode from density estimation
#'
#' Finds the mode using the [density] function and then obtains the maximum value.
#'
#' @param x Value vector. Numeric or integers.
#' @param adjust Bandwidth adjustment. See [density].
#'
#' @examples
#' x <- rchisq(1000, 3)
#' hist(x, br=50)
#' abline(v = dmode(x), col = "red")
#' abline(v = median(x), col = "green")
#' abline(v = mean(x), col = "blue")
#' @export
dmode <- function(x, adjust=1.5) {
  x <- trim_ends(x, 0.05, na.rm=TRUE)
  d <- density(x, n=1000, bw="SJ", adjust=adjust)
  d$x[which.max(d$y)]
}

#' Mode from counting frequency
#'
#' Finds the most frequent value from a vector of discrete values
#'
#' @param x an integer vector
#'
#' @return scalar integer value
#' @export
#'
#' @examples
#' cmode(rpois(1000, 20))
cmode <- function(x) {
  i <- as.integer(x)
  if (any(x - i != 0)) {
    warning("`x` not discrete. Converting vector with `as.integer`")
    x <- i
  }
  x <- sort(x)
  y <- rle(x)
  y$values[which.max(y$lengths)]
}

#' Trim extreme values at each end of a vector.
#'
#' @param x A [numeric] vector
#' @param trim Proportion of vector length to trim. Must be between 0 and 1.
#' E.g., a value 0.05 (default) trims 2.5\% off each end of a sorted vector.
#' @param na.rm omit `NA` values. May result in different size vector.
#'
#' @return A [numeric] vector in the original order of `x`, but with trimmed
#' values as `NA` if `na.rm=TRUE` or with these values removed if `FALSE`
#' (which will result in a different sized vector from the input).
#' @export
#' @examples
#' x <- rgamma(10000, 1, 1)
#' range(x)
#' length(x)     # <- 10000
#' sum(is.na(x)) # <- 0
#'
#' t <- trim_ends(x, trim = 0.1)
#' range(t)
#' length(t)     # <- 9000
#' sum(is.na(t)) # <- 0
#'
#' t <- trim_ends(x, 0.1, na.rm = FALSE)
#' range(t, na.rm = TRUE)
#' length(t)     # <- 10000
#' sum(is.na(t)) # <- 1000
trim_ends <- function(x, trim=0.05, na.rm=TRUE) {
  if (!is.numeric(trim)) {
    stop("`trim` must be a numeric value.")
  }

  if (trim < 0 || trim > 1) {
    stop("trim amount must be between 0 and 1")
  }

  is_na <- is.na(x)
  len_finite <- sum(!is_na)
  len_trim <- as.integer(floor(len_finite * (trim / 2)))

  # attributes
  which_na <- which(is_na)
  which_trimmed <- integer()
  trim_size <- len_trim * 2L

  if (len_trim > 0L) {
    if (len_finite - trim_size < 1) {
      warning("trim amount results in zero length vector or all NAs")
      x[] <- NA
    } else {
      x_i_sort <- order(x, na.last=TRUE)
      trim_index <- seq_len(len_trim)
      trim_index <- unique(c(trim_index, len_finite - trim_index + 1))
      which_trimmed <- sort(x_i_sort[trim_index])
      x[which_trimmed] <- NA
    }
  }

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  attr(x, "na.action") <- which_na
  attr(x, "trim.action") <- which_trimmed
  attr(x, "trim.size") <- trim_size
  x
}


# misc --------------------------------------------------------------------


prop_in_range <- function(x, lower, upper) {
  sum(x > lower & x < upper) / length(x)
}

probit_tbl <- function(x, k) {
  pct <- ecdf(x)(x)
  brks <- range_sequence(c(0, 1), k + 1)
  idx <- cut(pct, breaks=brks, include.lowest=TRUE, right=FALSE, labels=FALSE)
  data.table(qmin=brks[idx], qmax=brks[idx + 1], ecdf=pct)
}

rgamma2 <- function(n, mean=1, sd=1) {
  rgamma(n, (mean / sd)^2, mean / sd^2)
}

rnbinom2 <- function(n, mean=1, sd=1, min_size=1) {
  size <- max(min_size, mean^2 / (sd^2 - mean))
  rnbinom(n, size=size, mu=mean)
}

csum_left <- function(x, init=0) {
  cs <- cumsum(c(init, x))
  na.omit(shift(cs, 1, NA, "lag"))
}

norm_vec <- function(x, na.rm=TRUE) {
  limits <- range_no_inf(x, na.rm=na.rm)
  (x - limits[1]) / diff(limits)
}

norm_vec_sum <- function(x, na.rm=TRUE) {
  x / sum(x, na.rm=na.rm)
}

range_no_inf <- function(x, na.rm=TRUE) {
  if (all_missing(x)) {
    return(c(NA, NA))
  }

  range(x, na.rm=na.rm)
}

logit <- function(p) {
  p[p < 0 | p > 1] <- NA
  log(p / (1 - p))
}

sigmoid <- function(x) {
  # more computationally stable form of: 1 / (1 + exp(-x))
  # also: logistic(x, l=1, k=1)
  exp(-log(1 + exp(-x)))
}

col_adj <- function(c, a) {
  v <- log(exp(c + 1) * sqrt(a)) - 1
  pmax(pmin(v, 1), 0)
}

# Snap a value to either the min or max if outside some range
clip_range <- function(x, min=NULL, max=NULL) {
  if (!is.null(max)) {
    x <- pmin(x, max)
  }
  if (!is.null(min)) {
    x <- pmax(x, min)
  }
  return(x)
}
