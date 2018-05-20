# PositionSpread wrapper --------------------------------------------------

#' Spread Overlapping Grobs Spread overlapping groups by shrinking them to fit
#' within the data's `y` range.
#'
#' @param height One of `total` (default), `panel`, `single`, a numeric scalar
#'   value to give all geoms equal space, or a numeric vector the length of
#'   `panels*groups` for manually specifying the height of each group.
#' @param reverse Reverse the order of segments within overlapping `y` ranges.
#' @param padding Multiple of height. Will shrink/enlarge groups to fit within a
#'   region.
#' @return NULL
#' @export
#' @examples
#' TODO: geom_point(aes(group=myGroup), position_spread(height = 0.5))
position_spread <- function(height = NULL, reverse = FALSE, padding = 0.2) {
  ggproto(NULL, PositionSpread,
          height = height,
          reverse = reverse, padding = padding)
}

# ggproto object ----------------------------------------------------------

PositionSpread <- ggproto(
  "PositionSpread",
  Position,
  required_aes = "y",
  height = NULL,
  reverse = FALSE,
  padding = 0.2,

  setup_params = function(self, data) {
    height_pars <- standardize_height_param(data, self$height %NA% "single")

    list(
      height = height_pars$heights, custom = height_pars$custom,
      padding = self$padding %NA% 0, reverse = self$reverse %NA% FALSE)
  },

  compute_panel = function(data, params, scales) {
    force_dt(data) %>%
      transform_heights(
        heights = params$height,
        padding = params$padding) %>%
      spread_by_ymin(
        spreader = panel_spread, reverse = params$reverse,
        padding = params$padding) %>%
      rescale_overlapping(scales = scales, skip = params$custom) %>%
      set_col_order(c("PANEL", "group", "x", "y") %Names%
                      data) %>%
      as.data.frame()
  }
)

# subfunctions ------------------------------------------------------------

standardize_height_param <- function(data, height) {
  height_type <- c("num", "char")[c(is.numeric(height), is.character(height))]

  if (is_none(height_type)) {
    stop("Invalid height argument specified for position_spread: ",
         height,
         call. = FALSE)
  }

  assert_names(c("PANEL", "group", "y"), data)

  height_type <- height_type[1]

  heights <- switch(
    height_type,
    char = {
      grouping <- height_char_groups(height[1])

      # get all individual heights ("single")
      height_data <- set_range_data(data, "y",
                                    force_cols = TRUE,
                                    copy = FALSE) %>%
        # order by min and use only these variables
        .[order(.__min), .(PANEL, group, .__min, .__max, .__len)] %>%
        # unique data
        unique() %>%
        # group by common y min values
        .[, ygroup := .GRP, .(.__min)] %>%
        # space in between y groups (from min y to next groups min y)
        .[, space := unique_apply(.__min, ygroup, append_diff)] %>%
        # get last space for the top of the panel
        .[is.na(space), space := max(c(.[, max(space %NA% .__len,
                                               na.rm = TRUE)], .__len))] %>%
        # pick the smallest space according to user group
        .[, uspace := min(space), by = grouping] %>%
        # height is the max of user heights or y group space
        .[, ht := max(c(.__len, uspace)), by = c(grouping, "uspace")]

      rm_temp_cols(data)
      height_data$ht
    },
    num = {
      n_heights <- nrow(unique(data[, c("PANEL", "group")]))
      n_user_heights <- length(height)
      if (n_user_heights != 1 && n_user_heights != n_heights) {
        warning("Height length was size ", n_user_heights,
                " but ", n_heights, " heights are required. ",
                "Recycling heights if any are missing.",
                call. = FALSE)
      }

      rep_len(height, n_heights)
    },
    NULL
  )

  list(heights = heights, type = height_type, custom = height_type == "num")
}

height_char_groups <- function(height_str = c("total", "panel", "single")) {
  switch(tolower(match.arg(height_str)), total = NULL,
         panel = "PANEL", single = c("PANEL", "group"), NULL)
}

transform_heights <- function(data, heights, padding) {
  assert_names(c("group", "y"), data)

  force_dt(data) %>%
    set_range_data("y", names = ".__min", copy = FALSE) %>%
    .[, .__len := heights[.GRP], .(PANEL, group)] %>%
    rescale_groups("y") %>%
    rm_temp_cols()
}

spread_by_ymin <- function(data, spreader, ...) {
  assert_names(c("PANEL", "group", "y"), data)

  force_dt(data) %>%
    .[, ymin := min(y, na.rm = TRUE), .(PANEL, group)] %>%
    .[, spreader(.SD, ...), .(ymin, PANEL)] %>%
    .[, `:=`(ymin = NULL)] %>%
    .[]
}

# Spread overlapping interval.
# used as j arg in data.table
panel_spread <- function(SD, reverse = FALSE, padding = 0) {
  if (all(SD$group < 0)) {
    return(SD)
  }

  dt <- force_dt(SD, copy = TRUE) %>%
    set_range_data("y",
                   force_cols = TRUE,
                   names = c(".__min", ".__max", ".__len"), copy = FALSE)

  n_groups <- length(unique(dt$group))
  padding <- check_padding(dt$y, dt$.__len, padding)

  if (n_groups <= 1) {
    dt <- rescale_groups(dt, "y", scaler = padding) %>%
      rm_temp_cols()
    return(dt)
  }

  # stack order
  if ("x" %Names?% dt) {
    dt[, .__mid := -x]
  } else {
    dt[, .__mid := group]
  }

  dt <- dt[] %>%
    .[, .__mid := mean(range_no_inf(.__mid)), group] %>%
    .[order(-.__mid, -group, -y), ] %>%
    .[, `:=`(.__grp = .GRP), .(group)]

  # TODO fix reverse
  if (reverse) {
    dt <- dt[order(-.__grp), .__grp := .GRP, .__grp]
  }

  pad_adj <- 1 - (padding / max(1, n_groups - 2))

  dt[] %>%
    # total height of data
    .[, .__len := diff(range_no_inf(y))] %>%
    # each overlapping groups height
    .[, .__hg := .__max - .__min] %>%
    # group heights sum to total height
    .[, .__hn := .__len * unique_simplex(.__hg, .__grp)] %>%
    # shrink y by group height segments
    .[
      , .__y := rescale_as_other(y, c(0, .__hn * pad_adj)),
      .(.__grp, .__hn)
      ] %>%
    # reposition y by stacking based on group height values
    .[, .__csum := unique_apply(.__y * (1 / pad_adj), .__grp, csum_left)] %>%
    .[, y := .__y + .__csum + .__min] %>%
    # junk
    rm_temp_cols()
}

rescale_overlapping <- function(data, scales, skip = FALSE) {
  if (skip || is.null(scales$y) || !scales$y$is_discrete()) {
    # don't change size of continuous scales or height specified manually
    return(data)
  }

  o_laps <- get_overlaps(data, "y")

  complete_overlap <- o_laps$within

  if (all_missing(complete_overlap)) {
    return(data)
  }

  which_overlaps <- o_laps$overlap[!complete_overlap]

  if (any(complete_overlap)) {
    warning("Sizes completely overlap with given height.")
  }

  if (!any(which_overlaps)) {
    return(data)
  }

  dt <- set_range_data(data, "y", force_cols = TRUE, copy = FALSE)

  o_laps <- o_laps[which_overlaps, ]
  o_laps <- o_laps[order(o_laps$sep_height), ]
  o_laps <- o_laps[which.min(o_laps$space), ]

  min_y <- c(o_laps$min1, o_laps$min2)
  y_ord <- order(min_y)
  min_y <- min_y[y_ord]
  max_y <- c(o_laps$max1, o_laps$max2)[y_ord]
  h_y <- c(o_laps$h1, o_laps$h2)[y_ord][1]
  shrink <- (max_y[1] - min_y[2]) / h_y
  h_adj <- 1.2 * h_y * dt[
    group %in% c(o_laps$group1, o_laps$group2),
    1 - (resolution(y, FALSE) / diff(range_no_inf(y)))
    ]

  rm_temp_cols(rescale_groups(dt, "y", scaler = shrink, by = h_adj))
}
