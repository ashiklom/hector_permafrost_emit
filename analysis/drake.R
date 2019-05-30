#!/usr/bin/env Rscript
library(drake, exclude = c("expand", "gather", "plan"))
library(ggplot2, exclude = "ggsave")
library(dplyr)
library(tidyr)
library(readr)
library(parallel, include.only = "detectCores")
library(cowplot)
library(future)
library(here)

requireNamespace("ggpairs", quietly = TRUE)

devtools::load_all(".")
expose_imports("hector.permafrost.emit")

# To make this reproducible
draws <- read_csv(file.path("analysis", "data",
                            "derived_data", "parameter-draws.csv"))
if (!("--all" %in% commandArgs(trailingOnly = TRUE))) {
  draws <- head(draws, 100)
}
params <- rlang::syms(colnames(draws))

paper_file <- here("analysis", "paper", "paper.Rmd")

# Construct plan from components in "analysis/drake" directory
plan <- drake_plan()
drake_files <- list.files(here("analysis", "drake"), full.names = TRUE)
for (drake_file in drake_files) source(drake_file)

if (interactive()) {
  ## callr::rscript(here::here("analysis", "drake.R"), cmdargs = "--all")
  callr::rscript(here::here("analysis", "drake.R"))
} else {
  dconf <- drake_config(
    plan,
    parallelism = "future",
    jobs = parallel::detectCores(),
    prework = quote({
      devtools::load_all(".", quiet = TRUE)
    })
  )

  # Set number of cores depending on number of outdated tasks
  dout <- outdated(dconf)
  if (length(dout) > 10) {
    dconf[["jobs"]] <- parallel::detectCores()
    message("Number of outdated targets (", length(dout), ") ",
            "is greater than 10.",
            "Running in parallel across ", dconf[["jobs"]], " cores.")
  }

  make(config = dconf)
}
