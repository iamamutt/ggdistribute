# missing/null values -----------------------------------------------------

# check if x is NULL or zero length.
# if allow_na=TRUE then also accepts all NA values as none.
is_none <- function(x, allow_na = TRUE) {
  none <- is.null(x) || length(x) < 1
  if (allow_na) {
    return(none)
  }
  none || all(is.na(x))
}

# check if all values are length zero or NA
# wrapper for is_none(x, FALSE)
all_missing <- function(x) {
  is_none(x, allow_na = FALSE)
}

# try to grab first non NA value in x, if none, return NULL
first_non_na <- function(x) {
  val <- na.omit(x)[1] %:% NA
  if (is.na(val)) {
    return(NULL)
  }
  val
}

# return names of object that have non-zero chars
non_empty_names <- function(x, na.rm = TRUE) {
  obj_names <- names(x)
  obj_names <- obj_names[nzchar(obj_names)]
  if (!na.rm) {
    return(obj_names)
  }
  Filter(Negate(is.na), obj_names)
}

assert_names <- function(required, from_obj, ...) {
  err <- list(...)

  if (is_none(err)) {
    fr <- sys.call(-1)
    fr <- (is.null(fr)) %?% ".GlobalEnv" %:% deparse(fr[1])
    err <- paste0(fr, ".")
  } else {
    err <- do.call(paste0, err)
  }

  if (!(required %Names?% from_obj)) {
    stop("names missing: ", paste0(required, collapse = ", "),
      "\n", err,
      call. = FALSE)
  }
}

assert <- function(..., err = NULL, envir = parent.frame(), print) {
  exprs <- eval(substitute(alist(...)))

  force(envir)

  if (missing(print)) {
    if (is.null(err)) {
      print <- TRUE
    } else {
      print <- FALSE
    }
  }

  err <- paste0(err %:% "\b")

  lapply(
    exprs,
    function(e) {
      result <- eval(e, envir = envir)
      expr_str <- dQuote(deparse(e))

      if (print) {
        msg <- paste(err, "expression", expr_str, "failed")
      } else {
        msg <- err
      }

      if (!(length(result) == 1 && is.logical(result))) {
        stop("expression ", expr_str, " not logical", call. = FALSE)
      }

      if (!result) {
        stop(msg, call. = FALSE)
      }

      NULL
    })

  invisible()
}

# special operators -------------------------------------------------------

#' If true then object else null
#'
#' @details This is primarily used in conjuction with the `%:%` operator. See
#' examples.
#' @param lhs logical scalar.
#' @param rhs object to return if `lhs` is `TRUE`.
#' @return object on `rhs` or `NULL`
#' @examples
#' \dontrun{
#' TRUE  %?% 1 %:% 0  #> 1
#' FALSE %?% 1 %:% 0  #> 0
#'
#' x <- 0
#'
#' # expression returns whatever objects are wrapped in between %:%
#' (x == 0) %?% "y" %:% "n"
#' (x == 1) %?% c("y", "yes") %:% c("n", "No", "false")
#'
#' # ERROR: The %?% operator captured only 0, which is not logical.
#' x == 0 %?% "y" %:% "n"
#'
#' # ERROR: The TRUE slot cannot return NULL because NULL is used to
#' # decide what to return in %:%
#' #   `TRUE %?% NULL` and `FALSE %?% obj` would both return NULL.
#' (x == 0) %?% NULL %:% "n"
#' }
`%?%` <- function(lhs, rhs) {
  if (!is.logical(lhs) || length(lhs) > 1 || anyNA(lhs)) {
    stop("Value left of %?% must be logical, of length 1, and not NA.")
  }

  if (lhs) {
    if (is_none(rhs, allow_na = TRUE)) {
      stop(
        "Values right of %?% cannot return NULL or length zero.",
        " Try changing LHS to !LHS and reordering expresions after %?%.")
    }
    rhs
  } else {
    NULL
  }
}

# if LHS is NULL return RHS
`%:%` <- function(lhs, rhs) {
  if (is.null(lhs)) {
    rhs
  } else {
    lhs
  }
}

# if LHS is NULL or length zero return RHS
`%||%` <- function(lhs, rhs) {
  if (is_none(lhs, allow_na = TRUE)) {
    rhs
  } else {
    lhs
  }
}

# if LHS is NULL or length zero or all NA return RHS
`%NA%` <- function(lhs, rhs) {
  if (all_missing(lhs)) {
    rhs
  } else {
    lhs
  }
}

char_or_names <- function(x) {
  if (!is.character(x)) {
    x <- non_empty_names(x)
    if (is_none(x)) {
      stop("Could not determine names from object", call. = FALSE)
    }
  }
  x
}

`%Names%` <- function(lhs, rhs) {
  lhs <- char_or_names(lhs)
  lhs[lhs %chin% non_empty_names(rhs)]
}

`%!Names%` <- function(lhs, rhs) {
  lhs <- char_or_names(lhs)
  lhs[!(lhs %chin% non_empty_names(rhs))]
}

`%Names?%` <- function(lhs, rhs) {
  lhs <- char_or_names(lhs)
  all(lhs %chin% non_empty_names(rhs))
}

# transformations ---------------------------------------------------------

range_sequence <- function(x, n) {
  limits <- range_no_inf(x)
  if (all_missing(limits)) {
    return(rep(NA, n))
  }
  seq(limits[1], limits[2], length.out = n)
}

# rescale x to the scale of y
rescale_as_other <- function(x, y, scalar_adj = 1) {
  limits <- range_no_inf(y)
  if (all_missing(limits)) {
    warning("x left as is. No range detected from y.")
    return(x)
  }

  center <- mean(limits)
  scale <- diff(limits)

  # if the min/max of y is the same then x will start at y and end at y+1
  if (scale <= 0) {
    scale <- (scalar_adj == 0) %?% 1 %:% scalar_adj
    center <- center + scalar_adj * 0.5
  }

  center + (norm_vec(x) - 0.5) * scale
}

# data objects ------------------------------------------------------------

# data.frame/table is empty
empty <- function(x) {
  is.null(x) || nrow(x) == 0 || ncol(x) == 0
}

force_dt <- function(x, copy = FALSE) {
  if (data.table::is.data.table(x)) {
    if (copy) {
      return(data.table::copy(x))
    }
    x
  } else {
    data.table::as.data.table(x)
  }
}

set_col_order <- function(x, first_names) {
  cols <- names(x)
  not_first <- cols[!cols %chin% first_names]
  data.table::setcolorder(x, c(first_names, not_first))
  invisible(x)
}

rep.data.table <- function(x, ...) {
  args <- list(...)
  if (length(args) < 1) {
    stop("must provide a `times, lenght.out, or each` argument")
  }
  n <- c(args[[1]], args$times, args$length.out, args$each)[1]
  if (!is.numeric(n)) {
    stop("second input argument must be numeric")
  }
  rbindlist(lapply(
    seq_len(n),
    function(...) {
      x
    }))
}

get_static_data <- function(from, ref = NULL) {
  from <- force_dt(from)

  if (is.null(ref)) {
    use_names <- non_empty_names(from)
  } else {
    use_names <- from %!Names% ref
  }

  if (length(use_names) < 1) {
    return(NULL)
  }

  is_static_df <- from[, lapply(
    .SD,
    function(i) {
      uniqueN(i) == 1
    }), .SDcols = use_names]
  static_cols <- names(Filter(isTRUE, is_static_df))

  if (is_none(static_cols)) {
    return(NULL)
  }

  as.data.frame(from[1, static_cols, with = FALSE])
}

# misc --------------------------------------------------------------------

append_diff <- function(x, val = NA) {
  c(diff(x), val)
}


unique_apply <- function(x, i, FUN, ...) {
  if (is.null(i)) {
    i <- integer(length(x)) + 1L
  }

  if (!is.numeric(i)) {
    stop("i must be an integer index")
  }

  if (length(x) != length(i)) {
    stop("i must be the same size as x")
  }

  FUN <- match.fun(FUN)
  unq_i <- !duplicated(i)
  unq_v <- FUN(x[unq_i], ...)
  unq_v[i]
}

unique_simplex <- function(size_vec, grp_idx = NULL, offset_vec = 0) {
  size <- size_vec - offset_vec
  unique_apply(
    size,
    grp_idx,
    function(x) {
      x / sum(x)
    })
}

print_fn <- function(chunk_name, ...) {
  fns <- as.character(match.call())[-1L]
  txt <- capture.output(dump(fns[-1], ""))
  knitr::read_chunk(lines = txt, labels = fns[1])
}
