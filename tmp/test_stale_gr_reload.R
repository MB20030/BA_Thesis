library(tidyverse)
library(here)

source(here("data_cleaning_cantons", "00_functions.R"))
rm(allocate_hb_with_notes)
stopifnot(
  exists("allocate_hb", mode = "function"),
  !exists("allocate_hb_with_notes", mode = "function"),
  exists("finalize_election_data", mode = "function")
)

invisible(capture.output(source(
  here("data_cleaning_cantons", "canton_GR.r")
)))
stopifnot(exists("allocate_hb_with_notes", mode = "function"))

result <- process_canton_GR()
stopifnot(
  nrow(result) == 11L,
  sum(!is.na(result$hb_tie_note)) == 2L
)
cat("Stale-session reload guard passed.\n")
