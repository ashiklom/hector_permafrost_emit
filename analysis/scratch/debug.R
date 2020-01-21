devtools::load_all("~/Projects/hector_project/hector-permafrost-submodel")

rcp <- "26"
inidir <- file.path("~", "Projects", "hector_project",
                    "hector-permafrost-submodel", "inst", "input")
inifile <- normalizePath(file.path(inidir, paste0("hector_rcp", rcp, ".ini")))
stopifnot(file.exists(inifile))
hc <- newcore(inifile)
run(hc)
dates <- seq(1750, 2100)
outvars <- c(ATMOSPHERIC_CO2(), GLOBAL_TEMP())
no_pf <- fetchvars(hc, dates, outvars, scenario = "Original")
idate <- 1745
reset(hc, idate)
setvar(hc, idate, PERMAFROST_C(), 1035, "PgC")
fetchvars(hc, idate, PERMAFROST_C())
run(hc)
fetchvars(hc, idate, PERMAFROST_C())
yes_pf <- fetchvars(hc, dates, outvars, scenario = "Permafrost")
r26 <- rbind(no_pf, yes_pf)

library(ggplot2)
ggplot(r26) +
  aes(x = year, y = value, color = scenario) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free_y")
