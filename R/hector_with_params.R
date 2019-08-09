#' Run Hector with parameters set to specific values
#'
#' To specify units, set the `unit` attribute of the parameter.
#'
#' @param ... Named list of parameter values (e.g. `beta = 0.3`).
#'   Names must match Hector parameter names, or arguments to
#'   [split_biome()].
#' @param .dots A named list of arguments. Provides an alternative
#'   specification to `...`
#' @param rcp Representative carbon pathway (RCP) to use. One of
#'   `"26"`, `"45"`, `"60"`, `"85"`
#' @param core (Optional) An existing Hector core to modify.
#' @return Hector output, as a `data.frame`
#' @author Alexey Shiklomanov
#' @examples
#' hector_with_params(beta = 0.5, q10_rh = 1.8)
#' hector_with_params(
#'   default.beta = 0.6,
#'   permafrost.beta = 0.8,
#'   fveg_c = 0.2
#' )
#' @importFrom magrittr %>%
#' @export
hector_with_params <- function(..., .dots = list(), rcp = "45", core = NULL) {
  if (is.null(core)) {
    ini_file <- system.file(
      "input",
      paste0("hector_rcp", rcp, ".ini"),
      package = "hector"
    )
    stopifnot(file.exists(ini_file))
    core <- hector::newcore(
      ini_file,
      name = "sensitivity",
      suppresslogging = TRUE
    )
  }

  raw_params <- modifyList(.dots, list(...))
  all_names <- names(raw_params)
  sb_names_all <- names(formals(hector::split_biome))
  sb_names <- intersect(all_names, sb_names_all)
  sb_vars <- raw_params[sb_names]
  if (length(sb_names) > 0) {
    sb_input <- purrr::map(sb_vars, ~c(1 - .x, .x))
    invisible(rlang::exec(
      hector::split_biome,
      core = core,
      old_biome = "global",
      new_biomes = c("default", "permafrost"),
      !!!sb_input
    ))
  }

  param_names <- setdiff(all_names, sb_names_all)
  params <- raw_params[param_names]
  .iout <- tryCatch({
    purrr::iwalk(
      params,
      ~hector::setvar(core, NA, .y, .x, NA)
    )
  }, error = function(e) {
    message("Failed to set parameter value. ",
            "Probably hit this error during `reset`. ",
            "Returning `NULL`.")
    return(NULL)
  })
  if (is.null(.iout)) return(NULL)
  tryCatch({
    hector::run(core)
    hector::fetchvars(core, 2000:2100) %>%
      dplyr::mutate(!!!params, !!!sb_vars)
  }, error = function(e) {
    message("Run failed. Returning `NULL`.\n",
            "Hit the following error:\n",
            conditionMessage(e))
    return(NULL)
  })
}

set_parameter <- function(core, name, value) {
  unit <- attr(value, "unit")
  if (is.null(unit)) unit <- NA
  tryCatch(
    hector::setvar(core, NA, name, value, unit),
    error = function(e) {
      stop(sprintf(paste0(
        "Hit the following error while ",
        "trying to set parameter `%s`:\n%s"
      ), name, conditionMessage(e)))
    }
  )
}
