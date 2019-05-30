#!/usr/bin/env Rscript
library(dplyr)
library(readr)
library(hector.permafrost.emit)

set.seed(8675309)
ndraws <- 1000

npp_alpha <- c(f_nppv = 0.35, f_nppd = 0.60, f_npps = 0.05)
gl_npp_draws <- rdirichlet(ndraws, npp_alpha)[, 1:2] %>%
  `colnames<-`(., paste0("global.", colnames(.)))
pf_npp_draws <- rdirichlet(ndraws, npp_alpha)[, 1:2] %>%
  `colnames<-`(., paste0("permafrost.", colnames(.)))

draws <- tibble(
  global.beta = runif(ndraws, 0, 1),
  global.q10_rh = runif(ndraws, 0, 10),
  global.f_litterd = rbeta(ndraws, 0.98 * 4, 0.02 * 4),
  permafrost.beta = runif(ndraws, 0, 1),
  permafrost.q10_rh = runif(ndraws, 0, 10),
  permafrost.f_litterd = rbeta(ndraws, 0.98 * 4, 0.02 * 4),
  frac_veg = rbeta(ndraws, 1, 2),
  frac_soil = rbeta(ndraws, 1.1, 1.1),
  frac_detritus = rbeta(ndraws, 1.1, 1.1)
) %>%
  bind_cols(as_tibble(gl_npp_draws), as_tibble(pf_npp_draws))

write_csv(draws, file.path("analysis", "data",
                           "derived_data", "biome-parameter-draws.csv"))
