#!/usr/bin/env Rscript
library(dplyr)
library(readr)
library(clustermq)
library(fst)
library(here)
library(hector.permafrost.emit)

logdir <- here("logs")
dir.create(logdir, recursive = TRUE, showWarnings = FALSE)
outdir <- here("analysis", "data", "output")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

set.seed(8675309)
ndraws <- 5000

npp_alpha <- c(f_nppv = 0.35, f_nppd = 0.60, f_npps = 0.05)
npp_draws <- rdirichlet(ndraws, npp_alpha)[, 1:2]

draws <- tibble(
  beta = runif(ndraws, 0, 1),
  q10_rh = runif(ndraws, 0, 10),
  f_litterd = rbeta(ndraws, 0.98 * 4, 0.02 * 4)
) %>%
  bind_cols(as_tibble(npp_draws))

write_csv(draws, here("analysis", "data",
                      "derived_data", "parameter-draws.csv"))

hector_fun <- function(...) {
  library(hector.permafrost.emit)
  hector_with_params(...)
}

result <- Q_rows(draws, hector_fun, n_jobs = 50,
                 template = list(log_file = file.path(logdir, "global.log")))
result_df <- result %>%
  bind_rows() %>%
  as_tibble()

write_fst(result_df, file.path(outdir, "global-sims.fst"))
