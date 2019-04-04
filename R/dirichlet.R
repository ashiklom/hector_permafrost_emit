#' Dirichlet distribution
#'
#' A multivariate generalization of the Beta distribution.
#'
#' @param alpha Vector of `alpha` parameter values.
#' @inheritParams stats::dnorm
#' @inheritParams stats::rnorm
#' @return For `ddirichlet`, the kernel density. For `rdirichlet`, a
#'   matrix of draws with `n` rows and `length(alpha)` columns.
#' @author Alexey Shiklomanov
#' @export
ddirichlet <- function(x, alpha, log = FALSE) {
  if (is.matrix(x)) return(apply(x, 1, ddirichlet, alpha = alpha, log = log))
  if (length(x) != length(alpha)) {
    stop("Length x (", length(x), ") is not equal to ",
         "length alpha (", length(alpha), ").")
  }
  if (any (x < 0)) {
    warning("All x should be positive. Returning NA.")
    return(rep(NA_real_, length(x)))
  }
  if (abs(sum(x) - 1) > .Machine$double.eps) {
    warning("`sum(x)` is not 1 (within machine tolerance). Returning NA.")
    return(rep(NA_real_, length(x)))
  }
  log_f <- sum((alpha - 1) * log(x)) + lgamma(sum(alpha)) - sum(lgamma(alpha))
  if (log) return(log_f)
  exp(log_f)
}

#' @rdname ddirichlet
#' @export
rdirichlet <- function(n, alpha) {
  if (length(n) > 1) n <- length(n)
  normalize <- function(x) x / sum(x)
  samps <- vapply(alpha, function(x) rgamma(n, x, 1), numeric(n))
  if (n == 1) {
    matrix(normalize(samps), nrow = 1, ncol = length(samps))
  } else {
    t(apply(samps, 1, normalize))
  }
}
