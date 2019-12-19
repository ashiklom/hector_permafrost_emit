library(tidyverse)
library(fs)

filepath <- path("..", "cmip6", "cmip6.csv")
cmip6_raw <- read_csv(filepath, col_types = "cccccccccDDddDcd")

cmip6_raw %>%
  filter(variable_id == "npp") %>%
  count(experiment_id, source_id)

cmip6_raw %>%
  filter(
    experiment_id == "ssp585",
    ## source_id == "CanESM5",
    variable_id %in% c("npp", "ch4")
  ) %>%
  distinct(activity_id, institution_id, source_id, experiment_id,
           table_id, variable_id)

cmip6_raw %>%
  filter(variable_id %in% c("ch4", "npp"),
         experiment_id == "historical") %>%
  select(variable_id, everything()) %>%
  distinct(variable_id, source_id)

cmip6_raw %>%
  count(experiment_id) %>%
  print(n = Inf)

cmip6_raw %>%
  count(variable_id) %>%
  filter(variable_id %in% c("ch4", "npp"))

# Candidate models for calibration (have ch4 and npp):
# - BCC / BCC-CSM2-MR
# - CNRM-CERFACS / CNRM-ESM2-1
# - MOHC / UKESM1-0-LL
# - NCAR / CESM2-WACCM

link_root <- file.path("http://crd-esgf-drc.ec.gc.ca/thredds/fileServer/esgD_dataroot/AR6")

cmip6_raw %>%
  filter(source_id == "CanESM5", experiment_id == "ssp585", table_id == "Lmon",
         variable_id == "npp") %>%
  select(date, zstore)

cmip6_sub <- cmip6_

link <- "http://crd-esgf-drc.ec.gc.ca/thredds/fileServer/esgD_dataroot/AR6/CMIP6/ScenarioMIP/CCCma/CanESM5/ssp585/r2i1p2f1/Lmon/npp/gn/v20190429/npp_Lmon_CanESM5_ssp585_r2i1p2f1_gn_201501-210012.nc"

cmip6_raw[1, "zstore"]
