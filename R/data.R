#' Raw SRE dataset
#'
#' @format Dataset is an object of class [tibble::tibble].
#' @seealso [sre_data()]
#' @return A dataset of [tibble::tibble].
#' @examples
#' sre
#'
#' sre_data()
"sre"


#' @describeIn sre create samples from sre data
#' @param n number of samplers per effect and contrast
#' @param seed set.seed number
#' @export
sre_data <- function(n = 1000, seed = 19850519) {
  set.seed(seed)

  as_dtbl(sre) %>%
    .[
      , .(value = sample(value, n, replace = TRUE)),
      .(effect, contrast)
    ] %>%
    tibble::as.tibble()
}


#' Testing dataset of grouped Normal distributions
#'
#' @param mu Means for each group. A [numeric] vector with the length
#' corresponding to the number of groups.
#' @param n Number of observations for each group. Length 1 [integer].
#' @param sd_range The min and max to use for standard deviations. Length 2
#' @param seed A seed value to generate the same sample. [numeric] vector.
#' @return A [data.frame] with the following variables: `grp`, `cond`, `value`.
#' @export
#'
#' @examples
#' data_normal_sample(0, 100)
data_normal_sample <- function(mu = c(-0.5, 4), n = 500L,
                               sd_range = c(0.6, 1.4), seed = 19850519) {
  set.seed(seed)
  k <- clip_range(length(mu), max = 13)
  N <- n * k
  adj <- median(diff(sort(unique(c(0, mu))))) / max(1, (k - 1))
  adj <- rep(c(-adj, adj), each = ceiling(n / 2))[1:n]
  value <- unlist(lapply(
    mu,
    function(x) {
      rnorm(n,
        mean = x,
        sd = runif(1L,
          min = sd_range[1],
          max = sd_range[2])) + adj
    }))
  cond <- rep(LETTERS[seq_len(k)], each = n)
  grp <- sort(rep_len(letters[seq_len(k * 2)], N))
  tibble::tibble(grp, cond, value)
}


ggdist_data <- function(n = 500, j = 3, k = 2, na.rm = TRUE, seed = 20130110) {
  set.seed(seed)
  j <- max(min(j, 26), 1)
  k <- max(min(k, 26), 1)
  n_jk <- j * k
  n_i <- round(max(1, n / n_jk))

  dt <- expand.grid(j = seq_len(j), k = seq_len(k)) %>%
    as.data.table() %>%
    rep(n_i) %>%
    .[, `:=`(j_discrete = LETTERS[j], k_discrete = letters[k])] %>%
    .[order(j, k)] %>%
    .[, value := rnorm(.N, 2 * j, k^.618), .(j, k)] %>%
    cbind(.[, probit_tbl(value, n_jk)]) %>%
    .[, variable := runif(.N, qmin, qmax), .(qmin, qmax)] %>%
    .[, jk_i := .GRP, .(j, k)] %>%
    .[jk_i == max(jk_i), value := NA] %>%
    .[, .__v := min(variable, na.rm = TRUE)] %>%
    .[j == min(2, max(j)), variable := {
      variable[which.min(variable)] <- .__v[1]
      variable
    }] %>%
    .[order(j, k, variable)] %>%
    .[1, variable := min(.__v, na.rm = TRUE)] %>%
    .[, .__v := NULL] %>%
    .[, I := 1:.N]


  if (!na.rm) {
    return(tibble::as.tibble(dt))
  }

  dt[!is.na(value), ] %>%
    tibble::as.tibble()
}

diamonds_ggdistribute <- function(N = 1000,
                                  rdist = c("rnorm", "rnbinom2", "rgamma2"),
                                  na.rm = TRUE, seed = 19850519) {
  rdist <- match.fun(rdist[1])
  set.seed(seed)

  dt <- ggplot2::diamonds %>%
    as_dtbl() %>%
    .[, .(cut, color, clarity, carat, price)] %>%
    .[, price := as.numeric(price)] %>%
    melt(measure.vars = c("carat", "price")) %>%
    .[, na := ifelse(all_missing(value) | length(na.omit(value)) < 3,
      TRUE, FALSE), .(cut, color, clarity, variable)] %>%
    .[
      , .(
        center = all(na) %?% NA_real_ %:% dmode(value),
        scale = all(na) %?% NA_real_ %:% mad(value, na.rm = TRUE)),
      .(cut, color, clarity, variable)
    ] %>%
    .[, na := ifelse(is.na(center) | is.na(scale), TRUE, FALSE)] %>%
    .[
      , .(
        sample = all(na) %?% 0L %:% as.integer(1:N),
        value = all(na) %?% NA_real_ %:% rdist(N, center[1], scale[1])),
      .(cut, color, clarity, variable)
    ] %>%
    .[, value := trim_ends(value, .2, na.rm = FALSE), .(sample, variable)] %>%
    .[
      , minv := abs(min(0, min(value %NA% 0, na.rm = TRUE))),
      .(sample, cut, color, clarity, variable)
    ] %>%
    .[, value := value + minv] %>%
    .[, `:=`(minv = NULL)] %>%
    dcast(sample + cut + color + clarity ~ variable,
      value.var = "value",
      drop = FALSE, fill = NA, fun.aggregate = identity)

  if (!na.rm) {
    return(tibble::as.tibble(dt))
  }

  dt[!is.na(carat) & !is.na(price), ] %>%
    tibble::as.tibble()
}
