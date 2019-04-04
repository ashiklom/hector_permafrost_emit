#!/usr/bin/env Rscript
library(drake)
library(ggplot2)

requireNamespace("future", quietly = TRUE)

# begin imports
import::from("tibble", "tibble", "as_tibble", .into = "")
import::from("dplyr", "bind_rows", "bind_cols", "filter", "group_by",
             "summarize", "mutate", "ungroup", .into = "")
import::from("cowplot", "plot_grid", "theme_cowplot", "save_plot",
             .into = "")
import::from("parallel", "detectCores", .into = "")
import::from("magrittr", "%>%", .into = "")
import::from("tidyr", "gather", .into = "")
# end imports

devtools::load_all(".")
expose_imports("hector.permafrost.emit")

# To make this reproducible
set.seed(8675309)
ndraws <- 500
beta_draws <- runif(ndraws, 0, 1)
q10_draws <- runif(ndraws, 0, 10)

npp_alpha <- c(f_nppv = 0.35, f_nppd = 0.60, f_npps = 0.05)
npp_draws <- rdirichlet(ndraws, npp_alpha)[, 1:2]

draws <- tibble(
  beta = runif(ndraws, 0, 1),
  q10_rh = runif(ndraws, 0, 10),
  f_litterd = rbeta(ndraws, 0.98 * 4, 0.02 * 4)
) %>%
  bind_cols(as_tibble(npp_draws))

params <- rlang::syms(colnames(draws))

plan <- drake_plan(
  draws_plot = GGally::ggpairs(draws),
  sims = target(
    hector_with_params(
      beta = beta,
      q10_rh = q10_rh,
      f_nppv = f_nppv,
      f_nppd = f_nppd,
      f_litterd = f_litterd
    ),
    transform = map(!!!draws)
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
    transform = map(.y = !!params)
  ),
  ts_both = target(
    plot_grid(ts),
    transform = combine(ts)
  ),
  ts_both_png = save_plot(
    file_out(!!here::here("analysis", "figures", "ts_both.png")),
    ts_both
  ),
  # Scatter plot of values in 2100
  lastyear = filter(all_sims, year == 2100),
  sensitivity = lastyear %>%
    group_by(variable) %>%
    sensitivity_analysis(!!!params) %>%
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
  sensitivity_plot_png = save_plot(
    file_out(!!here::here("analysis", "figures", "sensitivity_plot.png")),
    sensitivity_plot
  ),
  scatter_common = ggplot(lastyear) +
    aes(y = value) +
    facet_wrap(vars(variable), scales = "free_y") +
    geom_point() +
    ylab("") +
    theme_bw(),
  scatter = target(
    scatter_common + aes(x = .x),
    transform = map(.x = !!params)
  ),
  scatter_both = target(
    plot_grid(scatter),
    transform = combine(scatter)
  ),
  scatter_both_png = save_plot(
    file_out(!!here::here("analysis", "figures", "scatter_both.png")),
    scatter_both
  )
)

is_callr <- Sys.getenv("CALLR") == "true"

dconf <- drake_config(
  plan,
  parallelism = "future",
  jobs = 1,
  prework = quote({
    devtools::load_all(".", quiet = TRUE)
  })
)

# Set number of cores depending on number of outdated tasks
dout <- outdated(dconf)
if (length(dout) > 10) {
  dconf[["jobs"]] <- parallel::detectCores()
  message("More than 10 outdated targets. ",
          "Running in parallel across ", dconf[["jobs"]], " cores.")
}

if (interactive()) {
  r_make(here::here("analysis", "drake.R"),
         r_args = list(env = c(callr::rcmd_safe_env(), "CALLR" = "true")))
} else if (is_callr) {
  dconf
} else {
  make(config = dconf)
}
