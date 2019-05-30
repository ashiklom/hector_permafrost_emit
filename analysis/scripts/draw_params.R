#!/usr/bin/env Rscript
library(dplyr)
library(readr)
library(hector.permafrost.emit)

set.seed(8675309)
ndraws <- 1000
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

write_csv(draws, file.path("analysis", "data",
                           "derived_data", "parameter-draws.csv"))
