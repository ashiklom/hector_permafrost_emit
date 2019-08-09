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
datadir <- here("analysis", "data", "derived_data")
dir.create(datadir)

set.seed(8675309)
ndraws <- 10000

npp_alpha <- c(f_nppv = 0.35, f_nppd = 0.60, f_npps = 0.05)
gl_npp_draws <- rdirichlet(ndraws, npp_alpha)[, 1:2] %>%
  `colnames<-`(., paste0("default.", colnames(.)))
pf_npp_draws <- rdirichlet(ndraws, npp_alpha)[, 1:2] %>%
  `colnames<-`(., paste0("permafrost.", colnames(.)))

draws <- tibble(
  default.beta = runif(ndraws, 0, 1),
  default.q10_rh = runif(ndraws, 0, 10),
  default.f_litterd = rbeta(ndraws, 0.98 * 4, 0.02 * 4),
  permafrost.beta = runif(ndraws, 0, 1),
  permafrost.q10_rh = runif(ndraws, 0, 10),
  permafrost.f_litterd = rbeta(ndraws, 0.98 * 4, 0.02 * 4),
  fveg_c = rbeta(ndraws, 1, 2),
  fsoil_c = rbeta(ndraws, 1.1, 1.1),
  fdetritus_c = rbeta(ndraws, 1.1, 1.1),
  ## fnpp_flux0 = rbeta(ndraws, 1.1, 1.1),
  permafrost.warmingfactor = 1 + 3 * rbeta(ndraws, 5, 5)
) %>%
  bind_cols(as_tibble(gl_npp_draws), as_tibble(pf_npp_draws))

write_csv(draws, file.path(datadir, "biome-parameter-draws.csv"))

hector_fun <- function(...) {
  library(hector.permafrost.emit)
  hector_with_params(...)
}

result <- Q_rows(draws, hector_fun,
                 const = list(biome_name = "permafrost"),
                 n_jobs = 50,
                 template = list(log_file = file.path(logdir, "biome.log")))
result_df <- result %>%
  bind_rows() %>%
  as_tibble()

write_fst(result_df, file.path(outdir, "biome-sims.fst"))
