#!/usr/bin/env Rscript
library(drake, exclude = c("expand", "gather", "plan"))
library(ggplot2, exclude = "ggsave")
library(dplyr)
library(tidyr)
library(readr)
library(parallel, include.only = "detectCores")
library(cowplot)
library(future)
library(here)

requireNamespace("ggpairs", quietly = TRUE)

devtools::load_all(".")
expose_imports("hector.permafrost.emit")

# To make this reproducible
draws <- read_csv(file.path("analysis", "data",
                            "derived_data", "parameter-draws.csv"))
if (!("--all" %in% commandArgs(trailingOnly = TRUE))) {
  draws <- head(draws, 100)
}
params <- rlang::syms(colnames(draws))

paper_file <- here("analysis", "paper", "paper.Rmd")

plan <- drake_plan(
  paper_md = rmarkdown::render(knitr_in(!!paper_file), "github_document"),
  paper_pdf = rmarkdown::render(knitr_in(!!paper_file), "pdf_document"),
  paper_html = rmarkdown::render(knitr_in(!!paper_file), "html_document"),
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

if (interactive()) {
  ## callr::rscript(here::here("analysis", "drake.R"), cmdargs = "--all")
  callr::rscript(here::here("analysis", "drake.R"))
} else {
  dconf <- drake_config(
    plan,
    parallelism = "future",
    jobs = parallel::detectCores(),
    prework = quote({
      devtools::load_all(".", quiet = TRUE)
    })
  )

  # Set number of cores depending on number of outdated tasks
  dout <- outdated(dconf)
  if (length(dout) > 10) {
    dconf[["jobs"]] <- parallel::detectCores()
    message("Number of outdated targets (", length(dout), ") ",
            "is greater than 10.",
            "Running in parallel across ", dconf[["jobs"]], " cores.")
  }

  make(config = dconf)
}
