devtools::load_all("~/Projects/hector_project/hector-permafrost-submodel")
library(tidyverse)

outvars_ch4 <- c(RF_CH4(), ATMOSPHERIC_CH4())
outvars_co2 <- c(ATMOSPHERIC_C(), ATMOSPHERIC_CO2())
outvars_both <- c(outvars_ch4, outvars_co2, GLOBAL_TEMP())
yrs <- 1750:2300
rcp45 <- system.file(
  "input", "hector_rcp45.ini",
  package = "hector"
)
hc <- newcore(rcp45)
invisible(run(hc))
results_default <- fetchvars(hc, yrs, outvars_both, scenario = "default")
setvar(hc, NA, RH_CH4_FRAC(), 0.004, NA)
setvar(hc, NA, NATURAL_CH4(), 0, "Tg CH4 year-1")
invisible(reset(hc))
invisible(run(hc))
results_methane <- fetchvars(hc, yrs, outvars_both, scenario = "methane")
results_both <- as_tibble(bind_rows(results_default, results_methane))
p1 <- ggplot(results_both) +
  aes(x = year, y = value, color = scenario) +
  geom_line() +
  geom_vline(xintercept = 2100, linetype = "dashed") +
  facet_wrap(vars(variable), scales = "free")

## ggsave("analysis/figures/dynamic-methane.png")

# Put it together with the permafrost model
outvars_both2 <- c(outvars_ch4, outvars_co2, GLOBAL_TEMP(),
                   ATMOSPHERIC_CO2(), GLOBAL_TEMP(), F_FROZEN(), SOIL_C(),
                   RH(), RH_SOIL(), RH_DETRITUS(),
                   VEG_C(), DETRITUS_C(), SOIL_C())
yrs <- 1750:2300
hc <- newcore(rcp45)
invisible(run(hc))
results_nopf <- fetchvars(hc, yrs, outvars_both2, scenario = "default")
setvar(hc, NA, RH_CH4_FRAC(), 0.004, NA)
setvar(hc, NA, NATURAL_CH4(), 0, "Tg CH4 year-1")
invisible(reset(hc))
setvar(hc, 0, PERMAFROST_C(), 1035, "PgC")
invisible(run(hc))
results_pf <- fetchvars(hc, yrs, outvars_both2, scenario = "permafrost")
results_allpf <- bind_rows(results_nopf, results_pf) %>%
  as_tibble()
p2 <- ggplot(results_allpf) +
  aes(x = year, y = value, color = scenario) +
  geom_line() +
  ## geom_vline(xintercept = 2100, linetype = "dashed") +
  facet_wrap(vars(variable), scales = "free") +
  ggtitle("RCP 4.5")
## p2 %+% filter(results_allpf, year < 1900)
p2

## ggsave("analysis/figures/pf-methane-rcp45.png")
