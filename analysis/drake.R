#!/usr/bin/env Rscript
if (getRversion() >= "3.6.0") {
  library(drake, exclude = c("expand", "gather", "plan"))
  library(ggplot2)
} else {
  library(drake)
  library(ggplot2)
}
library(dplyr)
library(tidyr)
library(readr)
library(cowplot)
library(future)
library(here)

stopifnot(
  requireNamespace("GGally", quietly = TRUE),
  requireNamespace("devtools", quietly = TRUE),
  requireNamespace("callr", quietly = TRUE)
)

pkgload::load_all(".", attach_testthat = FALSE)
expose_imports("hector.permafrost.emit")

# To make this reproducible
args <- commandArgs(trailingOnly = TRUE)
run_all <- "--all" %in% args
cluster <- "--cluster" %in% args
make <- "make" %in% args

paper_file <- here("analysis", "paper", "paper.Rmd")

# Construct plan from components in "analysis/drake" directory
plan <- drake_plan()
drake_files <- list.files(here("analysis", "drake"), full.names = TRUE)
for (drake_file in drake_files) source(drake_file)

if (cluster) {
  parallelism <- "clustermq"
  jobs <- 50
} else {
  parallelism <- "future"
  jobs <- parallel::detectCores()
}
dconf <- drake_config(
  plan,
  parallelism = parallelism,
  jobs = jobs,
  prework = quote({
    pkgload::load_all(".", attach_testthat = FALSE, quiet = TRUE)
  })
)

# Set number of cores depending on number of outdated tasks
dout <- outdated(dconf)
if (!cluster && length(dout) > 10) {
  dconf[["jobs"]] <- parallel::detectCores()
  message("Number of outdated targets (", length(dout), ") ",
          "is greater than 10. ",
          "Running in parallel across ", dconf[["jobs"]], " cores.")
}

if (make) {
  message("Running drake::make.")
  drake::make(config = dconf)
} else if (!interactive()) {
  dconf
}
