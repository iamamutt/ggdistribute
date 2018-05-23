ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

res_adjust <- function(x, scale_res = 1, zero = FALSE) {
  x + (resolution(x, zero) * scale_res)
}

#' scale and add
#'
#' @param base_size start value
#' @param amount multiple by
#' @param adj add after
#' @return numeric
scale_add <- function(base_size, amount = 1, adj = 0) {
  (base_size * amount) + adj
}

set_range_data <- function(data, axis = c("x", "y"), by = "group",
                           force_cols = FALSE, names = NULL, copy = TRUE) {
  axis <- match.arg(axis)
  do_ops <- structure(list(
    min,
    max,
    function(x) {
      diff(range(x))
    }), .Names = c(".__min", ".__max", ".__len"))

  if (!is.null(names)) {
    do_ops <- do_ops[seq_along(names)]
    names(do_ops) <- names
  }

  dt <- force_dt(data, copy = copy)

  Map(
    function(n, f) {
      if (force_cols || all_missing(data[[n]])) {
        dt[, eval(n) := f(get(axis)), by = eval(by)]
      }
    },
    names(do_ops),
    do_ops)

  if (copy) {
    as.data.frame(dt)
  } else {
    dt
  }
}

has_multiple_discrete <- function(data, scales, axis = c("x", "y")) {
  axis <- match.arg(axis)
  if (is.null(scales[[axis]]) || !scales[[axis]]$is_discrete()) {
    return(FALSE)
  }

  if (is.null(data[[axis]])) {
    return(FALSE)
  }

  length(unique(data[[axis]])) > 1
}

split_discrete_by_group <- function(data, scales, axis = c("x", "y")) {
  axis <- match.arg(axis)

  if (!("y" %Names?% data)) {
    return(data)
  }

  assert_names("group", data)
  dt <- force_dt(data)

  if (is.null(data[[axis]])) {
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
    .[, .__grp := NULL] %>%
    .[]
}

axis_is_within <- function(min1, max1, min2, max2) {
  m <- matrix(c(min1, min2, max1, max2, max1 - min1, max2 - min2), nrow = 2)
  m <- m[order(-m[, 3]), ]
  m[1, 1] <= m[2, 1] & m[1, 2] >= m[2, 2]
}

# Check for overlap
get_overlaps <- function(data, axis = c("x", "y"), tol = -1e-04) {
  axis <- match.arg(axis)
  assert_names(c(axis, "group"), data)
  dt <- force_dt(data)

  if (!("PANEL" %Names?% data)) {
    dt$PANEL <- 1L
  }

  minmax <- set_range_data(dt, axis, force_cols = TRUE, copy = FALSE) %>%
    .[
      !is.na(.__min) & !is.na(.__max), .(.__grp = .GRP),
      .(.__min, .__max, group)
    ]

  rm_temp_cols(dt)

  n_grps <- length(unique(minmax$group))

  if (n_grps == 1) {
    pairs <- matrix(c(1, 1), ncol = 2)
  } else {
    pairs <- t(utils::combn(n_grps, 2))
  }

  calc_vals <- list(
    pair_height = NA_real_, sep_height = NA_real_,
    space = NA_real_, space_ratio = NA_real_,
    scaled_space = NA_real_, overlap = NA, within = NA)

  overlap_data <- force_dt(pairs) %>%
    setnames(c("V1", "V2"), c("i1", "i2")) %>%
    .[, I := .I] %>%
    .[, `:=`(
      min1 = minmax$.__min[match(i1, minmax$.__grp)],
      max1 = minmax$.__max[match(i1, minmax$.__grp)],
      min2 = minmax$.__min[match(i2, minmax$.__grp)],
      max2 = minmax$.__max[match(i2, minmax$.__grp)],
      group1 = minmax$group[match(i1, minmax$.__grp)],
      group2 = minmax$group[match(i2, minmax$.__grp)])] %>%
    .[, `:=`(h1 = max1 - min1, h2 = max2 - min2)] %>%
    .[, `:=`(i1 = NULL, i2 = NULL)] %>%
    .[, eval(names(calc_vals)) := lapply(calc_vals, identity)]

  if (n_grps == 1) {
    return(as.data.frame(overlap_data))
  }

  overlap_data[] %>%
    .[
      , pair_height := max(max1, max2) - min(min1, min2),
      I
    ] %>%
    .[, sep_height := h1 + h2] %>%
    .[, space := pair_height - sep_height] %>%
    .[, space_ratio := pair_height / sep_height] %>%
    .[space < 0 | space_ratio < 1, scaled_space := scale(c(0, space))[-1]] %>%
    .[is.na(scaled_space), scaled_space := 0] %>%
    .[, overlap := scaled_space < tol] %>%
    .[, within := axis_is_within(min1, max1, min2, max2), I] %>%
    as.data.frame()
}

rescale_groups <- function(data, axis = c("x", "y"), min_col = ".__min",
                           ht_col = ".__len", scaler = 0, by = NULL) {
  axis <- match.arg(axis)
  assert_names(unique(c("group", min_col, ht_col, axis)), data)

  dt <- force_dt(data)
  dt[, .__by_ht := get(ht_col)]
  by_grp <- by

  if (is.numeric(by)) {
    dt[, .__by_ht := by]
    by_grp <- NULL
  }

  dt[] %>%
    .[, .__adj := scaler * max(.__by_ht), by = eval(by_grp)] %>%
    .[, eval(axis) := rescale_as_other(
      get(axis), c(get(min_col), get(ht_col) - .__adj + get(min_col))
    ),
    by = c("group", min_col, ht_col)
    ] %>%
    .[, `:=`(.__adj = NULL, .__by_ht = NULL)] %>%
    .[]
}

check_padding <- function(x, size, padding, mult = 1.5) {
  res <- resolution(x, FALSE) * mult
  max_ht <- max(size, na.rm = TRUE)
  shrunken_size <- max_ht - (padding * max_ht)

  if (shrunken_size > -res & shrunken_size < res) {
    warning("Paddding too large. Data compressed smaller than resolution.",
      call. = FALSE)
    return(0)
  }
  padding
}

rm_temp_cols <- function(data, temp_names = NULL) {
  if (is.null(temp_names)) {
    temp_names <- c(
      ".__min", ".__min_y", ".__max_y", ".__min_x", ".__max_x",
      ".__max", ".__mid", ".__grp", ".__len", ".__hg", ".__hn",
      ".__y", ".__csum", ".__tmp", ".__y_t", ".__y_b")
  }

  temp_names <- temp_names %Names% data

  if (is_none(temp_names)) {
    return(data)
  }

  force_dt(data) %>%
    .[, eval(temp_names) := NULL] %>%
    .[]
}

# unexported ggplot2 functions --------------------------------------------


find_subclass <- function(super, class, env) {
  name <- paste0(super, camelize(class, first = TRUE))
  obj <- find_global(name, env = env)
  if (is.null(name)) {
    stop("No ", tolower(super), " called ", name, ".", call. = FALSE)
  } else {
    if (!inherits(obj, super)) {
      stop("Found object is not a ", tolower(super), ".", call. = FALSE)
    }
  }
  obj
}

find_global <- function(name, env, mode = "any") {
  if (exists(name, envir = env, mode = mode)) {
    return(get(name, envir = env, mode = mode))
  }
  nsenv <- asNamespace("ggplot2")
  if (exists(name, envir = nsenv, mode = mode)) {
    return(get(name, envir = nsenv, mode = mode))
  }
  NULL
}

firstUpper <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}


camelize <- function(x, first = FALSE) {
  x <- gsub("_(.)", "\\U\\1", x, perl = TRUE)
  if (first) {
    x <- firstUpper(x)
  }
  x
}
