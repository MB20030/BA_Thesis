# One-time setup for this project. Run from the project root with:
# source("setup.R")

options(repos = c(CRAN = "https://cloud.r-project.org"))

required_packages <- c(
  "tidyverse",
  "ggrepel",
  "readxl",
  "janitor",
  "here",
  "writexl",
  "knitr",
  "rmarkdown",
  "quarto"
)

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

if (!file.exists("renv.lock")) {
  renv::init(bare = TRUE, restart = FALSE)
}

renv::install(required_packages)
renv::settings$snapshot.type("all")
renv::snapshot(prompt = FALSE)

message("Setup complete. Restart R before continuing.")
