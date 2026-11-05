# Avoid stale locks in renv's shared system-library sandbox on Windows.
# The project still uses the package library managed by renv.lock.
if (.Platform$OS.type == "windows") {
  options(renv.config.sandbox.enabled = FALSE)
}
source("renv/activate.R")
