#' Parameter sensitivity analysis
#'
#' Perform a PEcAn-like parameter sensitivity analysis to analyze the
#' contribution of parameters to predictive uncertainty.
#'
#' This analysis produces three key metrics:
#'
#' - "Coefficient of variation (CV)" describes the relative
#' uncertainty of the input parameter. It is calculated as the ratio
#' between the input parameter variance and its median value.
#'
#' - "Elasticity" is the normalized sensitivity of the model to a
#' change in one parameter.
#'
#' - "Partial variance" is the fraction of variance in the model
#' output that is explained by the given parameter. In essence, it
#' integrates the information provided by the CV and elasticity.
#'
#' The theory and implementation are based on the sensitivity analysis
#' described by [LeBauer et al.
#' (2013)](https://doi.org/10.1890/12-0137.1), but with several key
#' differences:
#'
#' - LeBauer et al. use a cubic spline interpolation through each
#' point in the model output. This function uses a generalized
#' additive model regression ([mgcv::gam()]).
#' - LeBauer et al. fit individual splines to each parameter-output
#' combination where other parameters are held constant at their
#' median. This function fits a multivariate generalized additive
#' regression model, and then uses that fit to calculate the partial
#' derivatives.
#'
#' @param df `data.frame` of Hector results.
#' @param xcols Character vector of parameter column names
#' @param ycol Name of column containing response variable
#' @param .type Whether the multivariate fit is "additive" (`Y ~ a + b +
#'   c`, default) or "interactive" (`Y ~ a * b * c`). This argument
#'   supports partial matching via [base::match.arg()].
#' @return `data.frame` of sensitivity analysis results. See Details.
#' @author Alexey Shiklomanov
#' @export
sensitivity_analysis <- function(dat, xcols, ycol, .type = "additive") {
  modtype <- match.arg(.type, c("additive", "interactive"))
  .collapse <- switch(modtype, additive = " + ", interactive = " * ")
  rhs <- paste(sprintf("s(%s)", xcols), collapse = .collapse)
  form <- paste(ycol, rhs, sep = " ~ ")
  fit <- mgcv::gam(as.formula(form), data = dat)
  xmed <- apply(dat[, xcols], 2, median)
  xcv <- (apply(dat[, xcols], 2, var) / xmed) %>%
    tibble::enframe("param", "cv")
  pred <- apply(dat[, xcols], 2, quantile, probs = c(0.49, 0.51))
  dpred <- apply(pred, 2, diff)
  ymed <- median(dat[[ycol]])

  # Elasticity
  el_inputs <- lapply(
    xcols,
    function(x) {
      as.data.frame(c(rlang::list2(
        !!x := pred[, x],
        !!!xmed[xcols != x]
      )))
    }
  )
  names(el_inputs) <- xcols
  el_results <- lapply(el_inputs, predict, object = fit)
  el_slope <- ((vapply(el_results, diff, numeric(1)) / dpred) /
                 (ymed / xmed)) %>%
    tibble::enframe("param", "elasticity")

  # Prediction variance
  pred_inputs <- lapply(
    xcols,
    function(x) {
      as.data.frame(c(rlang::list2(
        !!x := dat[[x]],
        !!!xmed[xcols != x]
      )))
    }
  )
  names(pred_inputs) <- xcols
  pred_results <- lapply(pred_inputs, predict, object = fit)
  pred_var <- vapply(pred_results, var, numeric(1)) %>%
    tibble::enframe("param", "pred_var") %>%
    dplyr::mutate(partial_var = pred_var / sum(pred_var))

  purrr::reduce(list(xcv, el_slope, pred_var), dplyr::full_join, by = "param")

}
