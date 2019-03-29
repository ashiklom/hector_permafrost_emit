#' Create a symbol from a combination of characters and symbols
#'
#' @param ... Characters or symbols. Each of these will be passed to [rlang::quo_name()]
#' @return A single symbol (see [rlang::sym()])
#' @author Alexey Shiklomanov
mksym <- function(...) {
  rlang::sym(paste0(purrr::map_chr(list(...), rlang::quo_name), collapse = ""))
}
