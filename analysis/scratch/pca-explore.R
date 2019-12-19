library(hector)
library(tidyverse)
library(fst)
library(data.table, exclude = c("between", "first", "last", "transpose"))

dat <- read_fst("analysis/data/output/global-sims.fst", as.data.table = TRUE)

dat[, param_id := .GRP, by = .(beta, q10_rh, f_litterd, f_nppv, f_nppd)]

params <- unique(dat[, .(param_id, beta, q10_rh, f_litterd, f_nppv, f_nppd)])

dat_pca <- dat %>%
  .[, .(param_id, year, variable, value)] %>%
  dcast(year + variable ~ param_id)

names(dat_pca) %>% head()

pca_mat <- as.matrix(dat_pca[, !"year"][, !"variable"])
pc_result <- princomp(t(pca_mat))

plot(pc_result$loadings[, 10], type = 'l')
