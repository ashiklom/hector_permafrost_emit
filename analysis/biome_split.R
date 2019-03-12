library(hector)

rcp45 <- read_ini(system.file("input", "hector_rcp45.ini", package = "hector"))
biome_ini <- modifyList(rcp45, list(simpleNbox = list(
  veg_c = 540,
  permafrost.veg_c = 10,

  detritus_c = 54,
  permafrost.detritus_c = 1,

  soil_c = 1600,
  permafrost.soil_c = 182,

  npp_flux0 = 49.0,
  permafrost.npp_flux0 = 1.0,

  ## permafrost.f_nppv = 0.35,
  ## permafrost.f_nppd = 0.60,
  ## permafrost.f_litterd = 0.98,
  ## permafrost.f_lucv = 0.1,
  ## permafrost.f_lucd = 0.01,
  permafrost.beta = 0.36,
  permafrost.q10_rh = 2.0
)))

tmp <- tempfile()
write_ini(biome_ini, tmp)

writeLines(readLines(tmp))

out <- runscenario(tmp)
