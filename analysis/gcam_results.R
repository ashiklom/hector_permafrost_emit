# devtools::install_github("jgcri/rgcam")
library(rgcam)
library(magrittr)
library(ggplot2)

## op <- options()
## options(error = function() traceback(2))

dbloc <- normalizePath("../test-gcam")
con <- localDBConn(dbloc, "database_basexdb")
qry <- system.file("ModelInterface", "sample-queries.xml", package = "rgcam")
prj <- addScenario(con, "testproject.dat", queryFile = qry)

co2 <- prj$Reference$`CO2 concentrations`

prj$Reference$`GDP by region`


co2 %>%
  dplyr::filter(value > 0) %>%
  ggplot() +
  aes(x = year, y = value) +
  geom_line()
