# Posterior layer wrapper -------------------------------------------------

#' @describeIn GeomPosterior geom_posterior Posterior Geom
#' @export
geom_posterior <- function(mapping=NULL, data=NULL, stat="DensityCI", position="spread",
                           ..., draw_ci=TRUE, draw_sd=TRUE, midline="#767698",
                           brighten=TRUE, mirror=FALSE, interp_thresh=NULL, na.rm=FALSE,
                           show.legend=NA, inherit.aes=TRUE) {
  layer(
    geom=GeomPosterior, stat=stat, data=data, mapping=mapping, position=position,
    show.legend=show.legend, inherit.aes=inherit.aes,
    params=list(
      draw_ci=draw_ci, draw_sd=draw_sd, midline=midline,
      mirror=mirror, brighten=brighten, interp_thresh=interp_thresh,
      na.rm=na.rm, ...
    )
  )
}

# ggproto object ----------------------------------------------------------

GeomPosterior <- ggproto(
  "GeomPosterior", Geom,
  default_aes=aes(
    weight=1, color="#585872", fill="#8989B2", size=0.5,
    alpha=NA, linetype="solid", vjust=1
  ),
  required_aes=c("x", "y"), non_missing_aes=character(),
  optional_aes=c("xmin", "cil", "sdl", "mid", "sdu", "ciu", "xmax"),
  draw_key=draw_key_polygon,


  setup_data=function(self, data, params) {
    if (!("y" %Names?% data) && "count" %Names?% data) {
      warning("Missing `y` in aesthetics. Defaulting to `count`", call.=FALSE)
      data$y <- data$count
    }

    mirror <- params$mirror %NA% FALSE

    if (is.null(params$vjust) && mirror) {
      params$vjust <- 0.5
    }

    compute_vjust(data, "y", params$vjust)
  },

  draw_group=function(self, data, ..., # panel_scales, coord,
                        draw_ci=TRUE, draw_sd=TRUE, midline="#767698",
                        brighten=TRUE, mirror=FALSE, interp_thresh=NULL) {
    if (nrow(data) == 1) {
      return(zeroGrob())
    }

    # handle NA options
    draw_ci <- draw_ci %NA% FALSE
    draw_sd <- draw_sd %NA% FALSE

    if (is.null(midline)) {
      midline <- first_non_na(data$colour)
    }

    midline <- midline %NA% NA
    brighten <- brighten %NA% FALSE
    mirror <- mirror %NA% FALSE

    warn <- is.finite(interp_thresh %:% Inf) %?% FALSE %:% TRUE
    interp_thresh <- interp_thresh %:% 0.01

    if (draw_ci) {
      assert_names(
        c("cil", "ciu"), data, "No confidence intervals found.",
        " Set `draw_ci=FALSE` to remove this segment or use ",
        "`stat_density_ci` to compute this variable automatically."
      )
    }

    if (draw_sd) {
      assert_names(
        c("sdl", "sdu"), data, "No SD intervals found.",
        " Set `draw_sd=FALSE` to remove this segment or use ",
        "`stat_density_ci` to compute this variable automatically."
      )
    }

    dt <- as_dtbl(data, copy=TRUE) %>%
      set_range_data("x", names=c("xmin", "xmax"), force_cols=FALSE, copy=FALSE) %>%
      set_range_data("y", names=c("ymin", "ymax"), force_cols=FALSE, copy=FALSE) %>%
      set_range_data(
        "y",
        names=c("grp_min", "grp_max"), force_cols=TRUE, copy=FALSE
      )

    params <- setup_posterior_params(
      dt,
      draw_ci=draw_ci, draw_sd=draw_sd, midline=midline,
      mirror=mirror, brighten=brighten, interp_thresh=interp_thresh, warn=warn
    )

    ggname(
      "geom_posterior",
      grid::gTree(children=do.call(grid::gList, c(
        get_posterior_segment_grobs(
          dt, params, ...
        ),
        get_posterior_line_grobs(
          dt, params, ...
        )
      )))
    )
  }
)

# subfunctions ------------------------------------------------------------

setup_posterior_params <- function(data, ...) {
  c(..., setup_post_seg_params(data, ...), setup_post_line_params(data, ...))
}

setup_post_seg_params <- function(data, brighten, draw_ci, draw_sd, ...) {
  fill <- first_non_na(data$fill) %NA% NA

  brighten <- if (is.logical(brighten)) {
    if (brighten) {
      c(4, 0, 1.333, 0, 4)
    } else {
      rep(0, 5L)
    }
  } else {
    rep_len(c(brighten, rev(brighten)[-1]), 5L)
  }

  segments <- c("xmin", "cil", "sdl", "sdu", "ciu", "xmax")

  if (!draw_ci) {
    segments[c(2, 5)] <- NA
    brighten[c(2, 4, 5)] <- NA
  }

  if (!draw_sd) {
    segments[c(3, 4)] <- NA
    brighten[c(3, 4)] <- NA
  }

  brighten <- na.omit(brighten)
  segments <- na.omit(segments)
  n <- length(brighten)
  segment_list <- structure(vector("list", n), .Names=segments[1:n])

  for (j in seq_len(n)) {
    cols <- segments[c(j, j + 1)]
    segment_list[[j]] <- list(
      lower_cut=first_non_na(data[[cols[1]]]) %:% NA_real_,
      upper_cut=first_non_na(data[[cols[2]]]) %:% NA_real_,
      fill=change_brightness(fill, brighten[j])
    )
  }

  c(list(segments=segment_list), fill=fill)
}

setup_post_line_params <- function(data, midline, draw_ci, draw_sd, ...) {
  colour <- first_non_na(data$colour) %NA% NA

  N <- 5
  line_pos <- c("cil", "sdl", "mid", "sdu", "ciu")
  line_widths <- c(1, 1.45, 2, 1.45, 1)
  line_colors <- rep(colour, N)
  line_colors[3] <- midline

  omit <- integer()

  if (!draw_ci) {
    omit <- append(omit, c(1, 5))
  }

  if (!draw_sd) {
    omit <- append(omit, c(2, 4))
  }

  keep <- which(!(seq_len(N) %in% omit))
  n <- length(keep)
  line_list <- structure(vector("list", n), .Names=line_pos[keep])

  for (j in seq_len(n)) {
    line_list[[j]] <- list(
      ci_column=line_pos[keep[j]], ci_color=line_colors[keep[j]],
      ci_lwd_adj=line_widths[keep[j]]
    )
  }

  c(list(lines=line_list), colour=colour)
}

get_posterior_data <- function(data, lower_cut=NULL, upper_cut=NULL, interp_thresh=NULL,
                               mirror=FALSE, colour="#000000", fill=NA, warn=TRUE) {
  dt <- compute_post_seg_data(data, lower_cut, upper_cut, interp_thresh, warn)

  # find the bottom and top parts of the distribution to make a complete line
  assert_names(c("ymin", "ymax", "grp_max", "grp_min"), data)

  if (mirror) {
    dt[, `:=`(ylower=grp_max / 2 + grp_min - y / 2, yupper=grp_max / 2 + y / 2)]

    adj <- 0
    adj <- dt[
      , .(adj=mean(c(ymax, ymin)) - mean(c(grp_min, grp_max))),
      .(grp_min, grp_max, ymin, ymax)
    ] %>%
      .[, adj]

    dt[, `:=`(ylower=ylower + adj, yupper=yupper + adj)]
  } else {
    dt[, `:=`(ylower=grp_min, yupper=y)]
  }

  rbind(
    copy(dt) %>%
      .[, y := ylower] %>%
      .[order(x)],
    copy(dt) %>%
      .[, y := yupper] %>%
      .[order(-x)]
  ) %>%
    rm_temp_cols(c("colour", "fill")) %>%
    .[, `:=`(colour=colour, fill=fill)] %>%
    rbind(.[1, ])
}

compute_post_seg_data <- function(data, lower_cut=NULL, upper_cut=NULL,
                                  interp_thresh=NULL, warn=TRUE) {
  assert_names(c("xmin", "xmax", "x", "y"), data)

  interp_thresh <- interp_thresh %NA% Inf

  if (is.finite(interp_thresh) && interp_thresh <= 0) {
    stop("`interp_thresh` must be greater than 0.")
  }

  dt <- as_dtbl(data, copy=TRUE) %>%
    .[order(x, y), .(x, y)]

  static <- get_static_data(data, dt)

  # use xmin, xmax if not specifying horiz. cutoff points
  if (all_missing(lower_cut)) {
    lower_cut <- first_non_na(static$xmin)
  }

  if (all_missing(upper_cut)) {
    upper_cut <- first_non_na(static$xmax)
  }

  # check that density data is within cut thresholds
  within_range <- dt[, x >= lower_cut & x <= upper_cut]
  if (!any(within_range)) {
    warning(sprintf(
      "No data found between the cutoff points: (%.2f, %.2f)", lower_cut,
      upper_cut
    ), call.=FALSE)
    return(cbind(dt[1, ], static))
  }

  # do further interpolation if cut points are too far away from x density data
  within_range <- which(within_range)

  if (!c("cil", "ciu") %Names?% static) {
    ci_width <- static$xmax - static$xmin
  } else {
    ci_width <- static$ciu - static$cil
  }

  lower_gap <- (dt[within_range, min(x)] - lower_cut) / ci_width
  upper_gap <- (upper_cut - dt[within_range, max(x)]) / ci_width

  if (lower_gap + upper_gap > interp_thresh * 2) {
    if (warn) {
      gap_info <- sprintf(
        paste0(
          "The left side at %.2f is missing %.2f%%,",
          " and the right side at %.2f is missing %.2f%%",
          " of the CI width."
        ), lower_cut, lower_gap * 100,
        upper_cut, upper_gap * 100
      )

      warning(
        "Interpolating gaps. ", gap_info, " Try setting `n` to a higher value,",
        " setting `interp_thresh` to a smaller value, ",
        "or do `interp_thresh=NA` to turn off interpolation completely.",
        call.=FALSE
      )
    }
    dxy <- interp_low_res(
      dt$x, dt$y, lower_cut, upper_cut, max(1 / interp_thresh, 1024)
    )
    cbind(dxy, static)
  } else {
    cbind(dt[within_range, ], static)
  }
}

compute_post_line_data <- function(data, ci_column, ci_color="#000000", ci_lwd_adj=1) {
  line_column <- data[[ci_column]]

  if (all_missing(line_column)) {
    return(as_dtbl(data[1, ]))
  }

  assert_names(c("ylower", "yupper", "x"), data)

  dt <- as_dtbl(data)
  vline <- first_non_na(line_column)

  grob_data <- data.table(
    x=c(vline, vline), y=interp_vert_line(
      dt$x, dt$ylower, dt$yupper, vline
    ),
    group=c(vline, vline)
  )

  other_data <- get_static_data(dt, grob_data)
  grob_data <- cbind(rep(as_dtbl(other_data), nrow(grob_data)), grob_data)
  grob_data$colour <- ci_color
  grob_data$size <- grob_data$size * ci_lwd_adj
  grob_data$fill <- NA
  grob_data
}


get_posterior_line_grobs <- function(data, params, ...) {
  outline_pars <- params[c("interp_thresh", "mirror", "colour")]
  outline_pars$data <- data
  outline_pars$fill <- NA
  lines_data <- do.call(get_posterior_data, outline_pars)

  # path grabs of vertical lines
  ci <- lapply(params$lines, function(j) {
    j$data <- lines_data
    grob_data <- do.call(compute_post_line_data, j)
    if (nrow(grob_data) < 2) {
      return(zeroGrob())
    }


    return(GeomPath$draw_panel(grob_data, ...))
  })

  # path grob of density outline
  if (nrow(lines_data) < 2) {
    dens <- zeroGrob()
  } else {
    dens <- grob_posterior(lines_data, ..., use_fill=FALSE, use_color=TRUE)
  }

  c(ci, list(dens))
}

# polygon grobs of distribution segments
get_posterior_segment_grobs <- function(data, params, ...) {
  shared_pars <- params[c("interp_thresh", "warn", "mirror")]
  shared_pars$colour <- NA
  shared_pars$data <- data

  lapply(params$segments, function(j) {
    segment <- do.call(get_posterior_data, c(shared_pars, j))

    grob_posterior(segment, ..., use_fill=TRUE, use_color=FALSE)
  })
}

grob_posterior <- function(data, ..., use_fill=FALSE, use_color=TRUE) {
  if (nrow(data) == 1) {
    return(zeroGrob())
  }

  munch_args <- structure(list(..., data), .Names=c("range", "coord", "data"))
  coords <- do.call(coord_munch, munch_args)

  # Sort by group to make sure that colors, fill, etc. come in same order
  coords <- coords[order(coords$group), ]
  first_rows <- coords[!duplicated(coords$group), ]

  ggname(
    "posterior_segment",
    grid::polygonGrob(
      coords$x, coords$y,
      default.units="native", id=coords$group,
      gp=grid::gpar(
        col=use_color %?% (first_rows$colour %:% NA) %:% NA,
        fill=use_fill %?% (first_rows$fill %:% NA) %:% NA,
        alpha=all_missing(first_rows$alpha) %?%
          1 %:% ((first_rows$alpha) %:% 1), lwd=first_rows$size,
        lex=.pt, lty=first_rows$linetype
      )
    )
  )
}

compute_vjust <- function(data, axis, vjust=NULL) {
  if (is.null(vjust)) {
    return(data)
  }

  dt <- set_range_data(data, axis, force_cols=TRUE, copy=FALSE)

  if (length(unique(dt$group)) == 1) {
    spacing <- dt[, median(.__len, na.rm=TRUE)]
  } else {
    spacing <- dt[] %>%
      .[, .__grp := .GRP, .__min] %>%
      .[, .__len := unique_apply(.__min, .__grp, append_diff, NA)] %>%
      .[, median(.__len, na.rm=TRUE)]
  }

  if (!is.na(spacing)) {
    dt[, y := y - (spacing * (1 - vjust))]
  }
  rm_temp_cols(dt)
}

# misc --------------------------------------------------------------------

interp_low_res <- function(x, y, from, to, n=128) {
  x_interp <- seq(from, to, length.out=n)
  interp_fun <- stats::approxfun(x, y)

  data.table(x=x_interp, y=interp_fun(x_interp))
}

interp_vert_line <- function(x, from, to, v) {
  c(stats::approxfun(x, from)(v), stats::approxfun(x, to)(v))
}
