draws <- read_csv(file.path("analysis", "data",
                            "derived_data", "parameter-draws.csv"))
if (!run_all) {
  draws <- head(draws, 100)
}
params <- rlang::syms(colnames(draws))
