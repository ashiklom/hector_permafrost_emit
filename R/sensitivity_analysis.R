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
#' @param ... Unquoted names of columns describing parameters to be
#'   used in sensitivity analysis.
#' @param .type Whether the multivariate fit is "additive" (`Y ~ a + b +
#'   c`, default) or "interactive" (`Y ~ a * b * c`). This argument
#'   supports partial matching via [base::match.arg()].
#' @return `data.frame` of sensitivity analysis results. See Details.
#' @author Alexey Shiklomanov
#' @export
sensitivity_analysis <- function(df, ..., .type = "additive") {
  .type <- match.arg(.type, c("additive", "interactive"))
  .collapse <- switch(.type,
                      "additive" = " + ",
                      "interactive" = " * ")

  params_q <- rlang::enquos(...)
  params_s <- purrr::map_chr(params_q, rlang::quo_name)
  pqs <- purrr::map(params_q, pq) %>% purrr::reduce(c)
  rhs <- paste(sprintf("s(%s)", params_s), collapse = .collapse)
  form <- paste0("value ~ ", rhs)
  pred_qs <- purrr::map(seq_along(params_q), pred_q, x = params_q) %>%
    purrr::reduce(c)
  name_vars <- purrr::map(params_s, mksym, "..var")
  partial_vars <- purrr::map(name_vars, ~rlang::quo(!!.x / total..var))
  names(partial_vars) <- paste0(params_s, "..partial_var")
  df %>%
    dplyr::summarize(
      !!!pqs,
      value_median = median(value),
      lfit = list(mgcv::gam(as.formula(form))),
      !!!pred_qs
    ) %>%
    dplyr::mutate(
      total..var = purrr::reduce(list(!!!name_vars), `+`),
      !!!partial_vars
    )
}

#' Clean up the output of the sensitivity analysis
#'
#' Remove extraneous columns, reshape the data to a long format, and
#' break up the output column names into meaningful pieces.
#'
#' @param sensitivity_out Output of [sensitivity_analysis()]
#' @return Tidy `data.frame` of sensitivity output
#' @export
tidy_sensitivity <- function(sensitivity_out) {
  result <- sensitivity_out %>%
    dplyr::select(
      -dplyr::ends_with("median"),
      -dplyr::ends_with("pred"),
      -dplyr::ends_with("df"),
      -lfit
    ) %>%
    tidyr::gather(result_type, value, -variable) %>%
    tidyr::separate(result_type, c("parameter", "stat"),
                    sep = "\\.\\.", extra = "merge")
}

#' Helper functions for performing the sensitivity analysis calculations
#'
#' These functions heavily leverage `rlang`'s quasiquotation mechanism
#' to build the code that will be used to perform the sensitivity
#' analysis.
#'
#' `pq` builds the
#'
#' @param param Unquoted name of a parameter
#' @param x Parameters in sensitivity analysis, as a list of symbols
#' @param i Index of target parameter for sensitivity calculations
#' @return Both functions return a list of quosures (via
#'   [rlang::quos()]), which are then spliced together in the main
#'   [sensitivity_analysis()] function.
#' @seealso [mksym()] for creating column names, [rlang::`!!()`] and [rlang::`!!!()`]
pq <- function(param) {
  param <- rlang::enquo(param)
  name_median <- mksym(param, "..median")
  name_cv <- mksym(param, "..cv")
  name_pred <- mksym(param, "..pred")
  rlang::quos(
    !!name_median := median(!!param),
    !!name_cv := var(!!param) / !!name_median,
    !!name_pred := list(!!name_median * c(0.99, 1.01))
  )
}

#' @rdname pq
pred_q <- function(x, i) {
  xchar <- purrr::map_chr(x, rlang::quo_name)
  xnames <- c(xchar[i], xchar[-i])
  first <- mksym(x[[i]], "..pred")
  then <- purrr::map(x[-i], mksym, "..median")
  l_dpred <- c(first, then)
  names(l_dpred) <- xnames
  name_dpred <- mksym(x[[i]], "..dpred")
  name_sens <- mksym(x[[i]], "..sens")
  name_sens_df <- mksym(x[[i]], "..sens_df")
  name_sens_pred <- mksym(x[[i]], "..sens_pred")
  name_elas <- mksym(x[[i]], "..elas")

  l_vpred <- c(x[[i]], then)
  names(l_vpred) <- xnames
  name_vpred <- mksym(x[[i]], "..vpred")
  name_vpred_df <- mksym(x[[i]], "..vpred_df")
  name_var <- mksym(x[[i]], "..var")

  first_median <- mksym(x[[i]], "..median")

  rlang::quos(
    # Sensitivity
    !!name_sens_df := list(data.frame(!!!l_dpred)),
    !!name_sens_pred := list(predict(lfit, !!name_sens_df)),
    !!name_sens := diff(!!name_sens_pred) / diff(!!first),
    # Elasticity = normalized sensitivity
    !!name_elas := !!name_sens / (value_median / !!first_median),
    # Prediction variance
    !!name_vpred_df := list(data.frame(!!!l_vpred)),
    !!name_vpred := list(predict(lfit, !!name_vpred_df)),
    !!name_var := var(!!name_vpred)
  )
}
