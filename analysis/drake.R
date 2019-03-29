#!/usr/bin/env Rscript
library(drake)
library(ggplot2)

requireNamespace("future", quietly = TRUE)

# begin imports
import::from("tibble", "as_tibble", .into = "")
import::from("dplyr", "bind_rows", "bind_cols", "filter", "group_by",
             "summarize", "mutate", "ungroup", .into = "")
import::from("cowplot", "plot_grid", "theme_cowplot", .into = "")
import::from("parallel", "detectCores", .into = "")
import::from("magrittr", "%>%", .into = "")
import::from("tidyr", "gather", .into = "")
# end imports

devtools::load_all(".")
expose_imports("hector.permafrost.emit")

# To make this reproducible
set.seed(8675309)
ndraws <- 150
beta_draws <- runif(ndraws, 0, 1)
q10_draws <- runif(ndraws, 0, 10)

hector_with_param <- function(beta, q10, rcp = "45") {
  ini_file <- system.file(
    "input",
    paste0("hector_rcp", rcp, ".ini"),
    package = "hector"
  )
  core <- hector::newcore(
    ini_file,
    name = "sensitivity",
    suppresslogging = TRUE
  )
  hector::setvar(core, NA, hector::BETA(), beta, NA)
  hector::setvar(core, NA, hector::Q10_RH(), q10, NA)
  hector::run(core)
  result <- hector::fetchvars(core, 2000:2100)
  result[["beta"]] <- beta
  result[["q10"]] <- q10
  result
}

plan <- drake_plan(
  params_df = bind_cols(beta = beta_draws, q10 = q10_draws),
  params_scatter = ggplot(params_df) +
    aes(x = beta, y = q10) +
    geom_point(alpha = 0.5),
  sims = target(
    hector_with_param(beta, q10),
    transform = map(beta = !!beta_draws, q10 = !!q10_draws)
  ),
  all_sims = target(
    as_tibble(bind_rows(sims, .id = "id")),
    transform = combine(sims)
  ),
  ts_common = ggplot(all_sims) +
    aes(x = year, y = value, group = id) +
    geom_line() +
    facet_wrap(vars(variable), scales = "free_y") +
    scale_color_gradient(low = "grey", high = "red") +
    ylab("") +
    theme_bw() +
    theme(
      legend.position = "bottom"
    ),
  ts = target(
    ts_common + aes(color = .y),
    transform = map(.y = c(beta, q10))
  ),
  ts_both = target(
    plot_grid(ts),
    transform = combine(ts)
  ),
  # Scatter plot of values in 2100
  lastyear = filter(all_sims, year == 2100),
  sensitivity = lastyear %>%
    group_by(variable) %>%
    sensitivity_analysis(beta, q10) %>%
    tidy_sensitivity(),
  sensitivity_plot = sensitivity %>%
    filter(
      parameter != "total",
      stat != "sens",
      stat != "var"
    ) %>%
    ggplot() +
    aes(x = parameter, y = value) +
    geom_segment(aes(x = parameter, y = 0, xend = parameter, yend = value)) +
    geom_point() +
    coord_flip() +
    facet_grid(cols = vars(stat), rows = vars(variable), scales = "free_x") +
    theme_bw(),
  scatter_common = ggplot(lastyear) +
    aes(y = value) +
    facet_wrap(vars(variable), scales = "free_y") +
    geom_point() +
    ylab("") +
    theme_bw(),
  scatter = target(
    scatter_common + aes(x = .x),
    transform = map(.x = c(beta, q10))
  ),
  scatter_both = target(
    plot_grid(scatter),
    transform = combine(scatter)
  )
)

is_callr <- Sys.getenv("CALLR") == "true"

dconf <- drake_config(
  plan,
  parallelism = "future",
  jobs = ifelse(is_callr, 1, parallel::detectCores()),
  prework = quote({
    devtools::load_all(".")
  })
)

if (interactive()) {
  r_make("analysis/drake.R",
         r_args = list(env = c(callr::rcmd_safe_env(), "CALLR" = "true")))
} else if (is_callr) {
  dconf
} else {
  make(config = dconf)
}
