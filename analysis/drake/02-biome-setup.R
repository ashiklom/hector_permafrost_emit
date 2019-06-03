## biome_draws <- read_csv(file.path("analysis", "data",
##                                   "derived_data", "biome-parameter-draws.csv"),
##                         col_types = cols(.default = "d"))
## if (!run_all) {
##   biome_draws <- head(biome_draws, 100)
## }
## biome_params <- rlang::syms(colnames(biome_draws))
