#' Print a small example plot with geom_posterior
#'
#' @param data A dataset to use, called at the top layer within `ggplot`.
#' @param x A character string of the `x` axis variable name (e.g., values
#' making up the distribution).
#' @param y A grouping variable for generating groups of distributions. Defaults
#' to `..count..` for no groups and displays the density as counts of the
#' number of samples for the value of `x`.
#' @param ... Additional arguments passed to [geom_posterior()].
#'
#' @return Object of class `gg`, `ggplot`.
#' @export
#' @examples
#' # Generate a basic example plot if no data is specified.
#' posterior_plot()
posterior_plot <- function(data, x, y="..count..", ...) {
  if (missing(data)) {
    data <- data_normal_sample(0)
    x <- "value"
    y <- "..count.."
  }

  ggplot(data, aes_string(x=x, y=y)) + geom_posterior(...)
}

ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

res_adjust <- function(x, scale_res=1, zero=FALSE) {
  x + (resolution(x, zero) * scale_res)
}

#' scale and add
#'
#' @param base_size start value
#' @param amount multiple by
#' @param adj add after
#' @return numeric
scale_add <- function(base_size, amount=1, adj=0) {
  (base_size * amount) + adj
}

set_range_data <- function(data, axis=c("x", "y"), by="group", force_cols=FALSE,
                           names=NULL, copy=TRUE) {
  axis <- match.arg(axis)
  do_ops <- structure(list(min, max, function(x) {
    diff(range(x))
  }), .Names=c(".__min", ".__max", ".__len"))

  if (!is.null(names)) {
    do_ops <- do_ops[seq_along(names)]
    names(do_ops) <- names
  }

  dt <- as_dtbl(data, copy=copy)

  Map(function(n, f) {
    if (force_cols || all_missing(data[[n]])) {
      dt[, eval(n) := f(get(axis)), by=eval(by)]
    }
  }, names(do_ops), do_ops)

  if (copy) {
    as.data.frame(dt)
  } else {
    dt
  }
}

has_multiple_discrete <- function(data, scales, axis=c("x", "y")) {
  axis <- match.arg(axis)
  if (is.null(scales[[axis]]) || !scales[[axis]]$is_discrete()) {
    return(FALSE)
  }

  if (is.null(data[[axis]])) {
    return(FALSE)
  }

  length(unique(data[[axis]])) > 1
}

split_discrete_by_group <- function(data, scales, axis=c("x", "y")) {
  axis <- match.arg(axis)

  if (!("y" %Names?% data)) {
    return(data)
  }

  assert_names("group", data)

  dt <- as_dtbl(data)

  if (is.null(dt[[axis]])) {
    dt <- dt[order(group)]
  } else {
    dt <- dt[order(get(axis), group)]
  }

  dt[, .__grp := {
    if (has_multiple_discrete(.SD, scales, axis)) {
      paste0(group, .SD[[axis]])
    } else {
      paste0(group, 1)
    }
  }, group] %>%
    .[, group := .GRP, .__grp] %>%
    rm_temp_cols()
}

axis_is_within <- function(min1, max1, min2, max2) {
  m <- matrix(c(min1, min2, max1, max2, max1 - min1, max2 - min2), nrow=2)
  m <- m[order(-m[, 3]), ]
  m[1, 1] <= m[2, 1] & m[1, 2] >= m[2, 2]
}

# Check for overlap
get_overlaps <- function(data, axis=c("x", "y"), tol=-1e-04) {
  axis <- match.arg(axis)
  assert_names(c(axis, "group"), data)
  dt <- as_dtbl(data)

  if (!("PANEL" %Names?% data)) {
    dt$PANEL <- 1L
  }

  minmax <- set_range_data(dt, axis, force_cols=TRUE, copy=FALSE) %>%
    .[!is.na(.__min) & !is.na(.__max), .(.__grp=.GRP), .(.__min, .__max, group)]

  rm_temp_cols(dt)

  n_grps <- length(unique(minmax$group))

  if (n_grps == 1) {
    pairs <- matrix(c(1, 1), ncol=2)
  } else {
    pairs <- t(utils::combn(n_grps, 2))
  }

  calc_vals <- list(
    pair_height=NA_real_, sep_height=NA_real_, space=NA_real_,
    space_ratio=NA_real_, scaled_space=NA_real_, overlap=NA, within=NA
  )

  # R CMD check
  h1 <- h2 <- i1 <- i2 <- max1 <- max2 <- min1 <- min2 <- NULL

  overlap_data <- as_dtbl(pairs) %>%
    setnames(c("V1", "V2"), c("i1", "i2")) %>%
    .[, I := .I] %>%
    .[, `:=`(
      min1=minmax$.__min[match(i1, minmax$.__grp)],
      max1=minmax$.__max[match(i1, minmax$.__grp)],
      min2=minmax$.__min[match(i2, minmax$.__grp)],
      max2=minmax$.__max[match(i2, minmax$.__grp)],
      group1=minmax$group[match(i1, minmax$.__grp)],
      group2=minmax$group[match(i2, minmax$.__grp)]
    )] %>%
    .[, `:=`("h1"=max1 - min1, "h2"=max2 - min2)] %>%
    .[, `:=`(i1=NULL, i2=NULL)] %>%
    .[, eval(names(calc_vals)) := lapply(calc_vals, identity)]

  if (n_grps == 1) {
    return(as.data.frame(overlap_data))
  }

  # R CMD check
  overlap <- pair_height <- scaled_space <- sep_height <- space <- space_ratio <- NULL

  overlap_data[] %>%
    .[, pair_height := max(max1, max2) - min(min1, min2), I] %>%
    .[, sep_height := h1 + h2] %>%
    .[, space := pair_height - sep_height] %>%
    .[, space_ratio := pair_height / sep_height] %>%
    .[space < 0 | space_ratio < 1, scaled_space := scale(c(0, space))[-1]] %>%
    .[is.na(scaled_space), scaled_space := 0] %>%
    .[, overlap := scaled_space < tol] %>%
    .[, within := axis_is_within(min1, max1, min2, max2), I] %>%
    as.data.frame()
}

rescale_groups <- function(data, axis=c("x", "y"), min_col=".__min", ht_col=".__len",
                           scaler=0, by=NULL) {
  axis <- match.arg(axis)
  assert_names(unique(c("group", min_col, ht_col, axis)), data)

  dt <- as_dtbl(data)
  dt[, .__by_ht := get(ht_col)]
  by_grp <- by

  if (is.numeric(by)) {
    dt[, .__by_ht := by]
    by_grp <- NULL
  }

  dt[] %>%
    .[, .__adj := scaler * max(.__by_ht), by=eval(by_grp)] %>%
    .[, eval(axis) := rescale_as_other(
      get(axis), c(get(min_col), get(ht_col) - .__adj + get(min_col))
    ),
    by=c("group", min_col, ht_col)
    ] %>%
    .[]
}

check_padding <- function(x, size, padding, mult=1.5) {
  res <- resolution(x, FALSE) * mult
  max_ht <- max(size, na.rm=TRUE)
  shrunken_size <- max_ht - (padding * max_ht)

  if (shrunken_size > -res & shrunken_size < res) {
    warning("Paddding too large. Data compressed smaller than resolution.", call.=FALSE)
    return(0)
  }
  padding
}

rm_temp_cols <- function(data, temp_names=NULL) {
  if (is.null(temp_names)) {
    temp_names <- c(
      ".__adj", ".__by_ht", ".__grp", ".__ht", ".__len", ".__max_x", ".__max_y",
      ".__max", ".__mid", ".__min_x", ".__min_y", ".__min", ".__n", ".__tmp", ".__y"
    )
  }

  temp_names <- temp_names %Names% data

  if (is_none(temp_names)) {
    return(data)
  }

  as_dtbl(data) %>%
    .[, eval(temp_names) := NULL] %>%
    .[]
}


# colorspace --------------------------------------------------------------


#' plot and show hex values of colors
#'
#' @param colors character vector of hex value colors
#' @param show.legend show the legend with hex values (logical)
#' @param ncols number of columns in the plot
#' @param alpha set alpha level for all colors
#'
#' @return A plot with the index of the color in the tile
#' @export
#'
#' @examples
#' show_colors(mejr_palette())
#' show_colors(topo.colors(25))
#' show_colors(cm.colors(64), FALSE)
#' show_colors(viridisLite::viridis(15), alpha = .8)
show_colors <- function(colors, show.legend=TRUE, ncols=NULL, alpha=NA) {
  if (missing(colors)) {
    colors <- rainbow(10)
  }

  n <- length(colors)

  if (n <= 0) {
    warning("no colors to plot")
  }

  if (!is.null(ncols)) {
    ncols <- max(c(1L, ncols))
  } else {
    ncols <- sqrt(n)
    if (ncols != as.integer(ncols)) {
      ncols <- 1L
    }
  }

  # R CMD check
  i <- x <- y <- z <- l <- NULL

  rows <- ceiling(n / ncols)
  data <- expand.grid(x=seq_len(ncols), y=seq_len(rows))
  data <- data[with(data, order(y, x)), ]
  data$z <- factor(rep_len(colors, nrow(data)), levels=colors, labels=colors)
  data$i <- rep_len(1:n, nrow(data))
  data$l <- paste(data$i, data$z, sep=". ")

  ggplot(data, aes(x, y, fill=z)) + geom_raster(aes(fill=z), alpha=alpha) +
    scale_fill_manual(values=as.character(colors), breaks=data$z, labels=data$l) +
    geom_label(fill="white", hjust=0, nudge_x=-.45, colour=gray(0.5), aes(label=i)) +
    scale_y_reverse() +
    theme_void() + guides(fill=guide_legend(override.aes=list(alpha=1))) +
    theme(
      legend.position=ifelse(show.legend, "right", "none"), legend.box="vertical",
      legend.box.background=element_blank(), legend.text.align=0,
      legend.direction="vertical", legend.title=element_blank()
    )
}

gray2 <- function(black=127) {
  if (is.character(black)) {
    return(black)
  }

  if (black < 0 | black > 255) {
    warning(simpleWarning("black out of range [0, 255]"))
  }

  gray(clip_range(black, 0, 255) / 255)
}


change_brightness <- function(hex_color, adjust) {
  if (adjust == 0 || all_missing(hex_color)) {
    return(hex_color)
  }

  rgb_mat <- clip_range(col2rgb(hex_color), 1, 254)
  rgb_adj <- sigmoid(logit(rgb_mat / 255) + adjust)

  apply(rgb_adj, 2, function(i) {
    do.call(rgb, as.list(i))
  })
}


# unexported ggplot2 functions --------------------------------------------


find_subclass <- function(super, class, env) {
  name <- paste0(super, camelize(class, first=TRUE))
  obj <- find_global(name, env=env)
  if (is.null(name)) {
    stop("No ", tolower(super), " called ", name, ".", call.=FALSE)
  } else {
    if (!inherits(obj, super)) {
      stop("Found object is not a ", tolower(super), ".", call.=FALSE)
    }
  }
  obj
}

find_global <- function(name, env, mode="any") {
  if (exists(name, envir=env, mode=mode)) {
    return(get(name, envir=env, mode=mode))
  }
  nsenv <- asNamespace("ggplot2")
  if (exists(name, envir=nsenv, mode=mode)) {
    return(get(name, envir=nsenv, mode=mode))
  }
  NULL
}

firstUpper <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep="")
}


camelize <- function(x, first=FALSE) {
  x <- gsub("_(.)", "\\U\\1", x, perl=TRUE)
  if (first) {
    x <- firstUpper(x)
  }
  x
}
