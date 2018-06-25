# PositionSpread wrapper --------------------------------------------------

#' Spread Overlapping Grobs Spread overlapping groups by shrinking them to fit
#' within the data's `y` range.
#'
#' @param height One of `total` (default), `panel`, `single`, a numeric scalar
#' value to give all geoms equal space, or a numeric vector the length of
#' `panels*groups` for manually specifying the height of each group.
#' @param reverse Reverse the order of segments within overlapping `y` ranges.
#' @param padding Multiple of height. Will shrink/enlarge groups to fit within a
#' region.
#' @return NULL
#' @export
#' @examples
#' library(ggplot2)
#'
#' x <- data.frame(y = rnorm(1000), x="", myGroup=sample(1:3, 1000, TRUE))
#'
#' ggplot(x, aes(x, y))+
#' geom_point(aes(group=myGroup), position=position_spread(height = 0.5))
position_spread <- function(height=NULL, reverse=FALSE, padding=0.2) {
  ggproto(NULL, PositionSpread, height=height, reverse=reverse, padding=padding)
}

# ggproto object ----------------------------------------------------------

PositionSpread <- ggproto(
  "PositionSpread",
  Position,
  required_aes="y",
  height=NULL,
  reverse=FALSE,
  padding=0.2,

  setup_params=function(self, data) {
    height_pars <- standardize_height_param(data, self$height %NA% "single")

    list(
      height=height_pars$heights, custom=height_pars$custom,
      padding=self$padding %NA% 0, reverse=self$reverse %NA% FALSE
    )
  },

  compute_panel=function(data, params, scales) {
    as_dtbl(data) %>%
      transform_heights(
        heights=params$height,
        padding=params$padding
      ) %>%
      spread_by_ymin(
        spreader=shrink_and_stack, reverse=params$reverse,
        padding=params$padding
      ) %>%
      rescale_overlapping(scales=scales, skip=params$custom) %>%
      set_col_order(c("PANEL", "group", "x", "y") %Names% data) %>%
      as.data.frame()
  }
)

# subfunctions ------------------------------------------------------------
standardize_height_param <- function(data, height) {
  height_type <- c("num", "char")[c(is.numeric(height), is.character(height))]

  if (is_none(height_type)) {
    stop("Invalid height argument specified for position_spread: ", height, call.=FALSE)
  }

  assert_names(c("PANEL", "group", "y"), data)

  # R CMD check
  space <- uspace <- NULL

  height_type <- height_type[1]
  n_heights <- nrow(unique(data[, c("PANEL", "group")]))
  dt <- as_dtbl(data, copy=TRUE)

  heights <- switch(
    height_type,
    char={
      grouping <- height_char_groups(height[1])

      # check if only one group per panel
      lonely_groups <- dt[] %>%
        set_range_data("y",
          by=c("group", "PANEL"),
          force_cols=TRUE, copy=FALSE) %>%
        .[, .(.__n=length(unique(.__min))), PANEL] %>%
        .[, all(.__n == 1)]

      ht <- if (lonely_groups) {
        height <- dt[, .(.__len=max(.__max - (y - .__min))), PANEL] %>%
          .[, max(.__len, na.rm=TRUE)]

        rep_len(height, n_heights)
      } else {
        # get all individual heights ("single")
        height_data <- set_range_data(dt, "y", force_cols=TRUE, copy=FALSE) %>%
          # order by min and use only these variables
          .[order(.__min), .(PANEL, group, .__min, .__max, .__len)] %>%
          # unique data
          unique() %>% # group by common y min values
          .[, .__grp := .GRP, .(.__min)] %>%
          # space in between y groups (from min y to next groups min y)
          .[, space := unique_apply(.__min, .__grp, append_diff)] %>%
          # get last space for the top of the panel
          .[
            is.na(space),
            space := max(c(.[, max(space %NA% .__len, na.rm=TRUE)], .__len))
          ] %>%
          # pick the smallest space according to user group
          .[, uspace := min(space), by=grouping] %>%
          # height is the max of user heights or y group space
          .[, ht := max(c(.__len, uspace)), by=c(grouping, "uspace")]

        height_data$ht
      }

      ht
    },
    num={
      n_user_heights <- length(height)
      if (n_user_heights != 1 && n_user_heights != n_heights) {
        warning("Height length was size ", n_user_heights, " but ",
          n_heights, " heights are required. ",
          "Recycling heights if any are missing.",
          call.=FALSE)
      }

      rep_len(height, n_heights)
    },
    NULL
  )

  heights <- dt[, .(`.__ht`=heights[.GRP]), .(PANEL, group)]
  rm_temp_cols(data)
  list(heights=heights, type=height_type, custom=height_type == "num")
}

height_char_groups <- function(height_str=c("total", "panel", "single")) {
  switch(tolower(match.arg(height_str)), total=NULL, panel="PANEL",
  single=c("PANEL", "group"), NULL)
}

transform_heights <- function(data, heights, padding) {
  assert_names(c("group", "y"), data)

  as_dtbl(data) %>%
    merge(heights, by=c("PANEL", "group"), all.y=FALSE) %>%
    set_range_data("y", names=".__min", copy=FALSE) %>%
    rescale_groups("y", ht_col=".__ht") %>%
    rm_temp_cols()
}

spread_by_ymin <- function(data, spreader, ...) {
  assert_names(c("PANEL", "group", "y"), data)

  # NOTE: also creates the variables ymin, ymax in data
  as_dtbl(data) %>%
    set_range_data("y",
      by=c("PANEL", "group"),
      force_cols=TRUE, names=c("ymin", "ymax"),
      copy=FALSE) %>%
    .[, .__min := ymin] %>%
    .[, spreader(.SD, ...), .(.__min, PANEL)] %>%
    rm_temp_cols()
}

# shrink each group to fit within y range and stack on top of each other.
# groups should be overlapping
shrink_and_stack <- function(SD, reverse=FALSE, padding=0) {
  if (all(SD$group < 0)) {
    return(SD)
  }

  assert_names(c("ymin", "ymax"), SD)

  # must copy if as j within data.table
  dt <- as_dtbl(SD, copy=TRUE)
  dt[, .__len := ymax - ymin]

  n_groups <- length(unique(dt$group))
  padding <- check_padding(dt$y, dt$.__len, padding)

  if (n_groups <= 1) {
    return(rm_temp_cols(rescale_groups(dt, "y", min_col="ymin", scaler=padding)))
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
    .[, `:=`(.__grp=.GRP), .(group)]

  if (reverse) {
    dt <- dt[order(-.__grp), .__grp := .GRP, .__grp]
  }

  pad_adj <- 1 - (padding / max(1, n_groups - 2))

  dt[] %>% # total height of data
    .[, .__len := diff(range_no_inf(y))] %>% # each overlapping groups height
    .[, .__ht := ymax - ymin] %>%
    # group heights sum to total height
    .[, .__len := .__len * unique_simplex(.__ht, .__grp)] %>%
    # shrink y by group height segments
    .[, .__y := rescale_as_other(y, c(0, .__len * pad_adj)), .(.__grp, .__len)] %>%
    # reposition y by stacking based on group height values
    .[, .__tmp := unique_apply(.__y * (1 / pad_adj), .__grp, csum_left)] %>%
    .[, y := .__y + .__tmp + ymin] %>%
    .[, `:=`(ymin=min(y), ymax=min(y) + .__len), .(.__grp)] %>% # junk
    rm_temp_cols()
}

rescale_overlapping <- function(data, scales, skip=FALSE) {
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

  dt <- set_range_data(data, "y", force_cols=TRUE, copy=FALSE)

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

  rm_temp_cols(rescale_groups(dt, "y", scaler=shrink, by=h_adj))
}
