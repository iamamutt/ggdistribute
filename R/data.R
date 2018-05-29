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

  # R CMD check
  effect <- contrast <- value <- NULL

  as_dtbl(get("sre")) %>%
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
#' @return A [data.frame] with the following variables: `Group`, `Condition`,
#' `value`.
#' @export
#' @examples
#' data_normal_sample(0, 100)
data_normal_sample <- function(mu = c(-0.5, 4), n = 500L,
                               sd_range = c(0.6, 1.4), seed = 19850519) {
  set.seed(seed)

  k <- clip_range(length(mu), max = 13)
  N <- n * k

  if (k == 1) {
    adj <- 0
  } else {
    adj <- median(diff(sort(unique(c(0, mu))))) / max(1, (k - 1))
  }

  adj <- c(-adj, adj)
  adj_len <- rep(adj, each = ceiling(n / 2))[1:n]

  value <- unlist(lapply(
    mu,
    function(x) {
      rnorm(n,
        mean = x,
        sd = runif(1L,
          min = sd_range[1],
          max = sd_range[2])) + adj_len
    }))

  scores <- unlist(lapply(
    mu,
    function(x) {
      sort(runif(2L, x + adj[1], x + adj[2]))
    }))

  Condition <- rep(LETTERS[seq_len(k)], each = n)
  Group <- sort(rep_len(letters[seq_len(k * 2)], N))
  GroupScore <- rep(scores, each = ceiling(N / k / 2))[1:N]

  tibble::tibble(Condition, Group, GroupScore, value)
}

ggdist_data <- function(n = 500, j = 3, k = 2, na.rm = TRUE, seed = 20130110) {
  set.seed(seed)
  j <- max(min(j, 26), 1)
  k <- max(min(k, 26), 1)
  n_jk <- j * k
  n_i <- round(max(1, n / n_jk))

  # R CMD check
  jk_i <- qmin <- qmax <- value <- variable <- NULL

  dt <- expand.grid(j = seq_len(j), k = seq_len(k)) %>%
    as.data.table() %>%
    rep(n_i) %>%
    .[, `:=`(j_discrete = LETTERS[j], k_discrete = letters[k])] %>%
    .[order(j, k)] %>%
    .[, value := rnorm(.N, 2 * j, k^.618), .(j, k)] %>%
    cbind(.[, probit_tbl(value, n_jk)]) %>%
    .[, variable := runif(.N, qmin, qmax), .(qmin, qmax)] %>%
    .[, `:=`(`jk_i` = .GRP), .(j, k)] %>%
    .[`jk_i` == max(`jk_i`), value := NA] %>%
    .[, .__tmp := min(variable, na.rm = TRUE)] %>%
    .[j == min(2, max(j)), variable := {
      variable[which.min(variable)] <- .__tmp[1]
      variable
    }] %>%
    .[order(j, k, variable)] %>%
    .[1, variable := min(.__tmp, na.rm = TRUE)] %>%
    .[, .__tmp := NULL] %>%
    .[, I := 1:.N]


  if (!na.rm) {
    return(tibble::as.tibble(dt))
  }

  dt[!is.na(value), ] %>%
    tibble::as.tibble()
}
