# Graubünden-specific data cleaning and processing
# Processes the 2022 and 2026 elections and returns the standardized cleaned table

library(tidyverse)
library(readxl)
library(here)

# Make all file references independent of the current working directory.
here::i_am("data_cleaning_cantons/canton_GR.r")

# Load shared allocation and output-format helpers when this file runs alone.
if (!exists("allocate_hb", mode = "function") ||
    !exists("finalize_election_data", mode = "function")) {
  source(here("data_cleaning_cantons", "00_functions.R"))
}

standardize_gr_party <- function(party_label) {
  party_label |>
    # Remove the workbook prefix (for example, "Listengruppe 03").
    str_remove("^Listengruppe\\s+\\d+\\s*") |>
    str_squish() |>
    str_to_upper() |>
    # Convert German umlauts to stable ASCII identifiers.
    str_replace_all(c(
      "Ä" = "AE",
      "Ö" = "OE",
      "Ü" = "UE"
    )) |>
    # Merge labels that represent the same party or electoral alliance.
    recode(
      "MITTE" = "DIEMITTE",
      "MITTE GR" = "DIEMITTE",
      "SP & GRUENE" = "SP_GRUENE"
    )
}

# Read the Canton's Election Data (Kanton Graubünden, 2022, 2026)
read_gr_election <- function(file_name, election_year) {
  # Import the two sheets needed for vote totals and elected candidates.
  results_raw <- readxl::read_excel(
    here("data", "datasets", "graubuenden", file_name),
    sheet = "Ergebnisse nach Gemeinden",
    col_names = FALSE,
    na = c("", "..."),
    .name_repair = ~ paste0("x", seq_along(.x))
  )

  candidates_raw <- readxl::read_excel(
    here("data", "datasets", "graubuenden", file_name),
    sheet = "Kandidaten",
    col_names = FALSE,
    na = c("", "..."),
    .name_repair = ~ paste0("x", seq_along(.x))
  )

  # Row 3 identifies every party-vote column; its right-hand neighbour stores
  # that party's canton-wide vote share.
  header_values <- as.character(unlist(results_raw[3, ], use.names = FALSE))
  party_vote_cols <- which(str_detect(header_values, "^Listengruppe"))
  party_share_cols <- party_vote_cols + 1L
  party_vote_names <- names(results_raw)[party_vote_cols]
  list_numbers <- str_extract(header_values[party_vote_cols], "\\d+") |>
    str_pad(width = 2, side = "left", pad = "0")
  parties <- standardize_gr_party(header_values[party_vote_cols])

  # Fail early if the workbook structure no longer matches these assumptions.
  stopifnot(
    length(party_vote_cols) == length(parties),
    !anyDuplicated(parties),
    all(party_share_cols <= ncol(results_raw))
  )

  party_lookup <- tibble(
    list_number = list_numbers,
    party = parties
  )

  # Reconstruct official Double-Pukelsheim seats by counting elected candidates
  # per list (column 2 = list number; column 9 = election status).
  official_results <- candidates_raw[-c(1:3), c(2, 9)] |>
    set_names(c("list_number", "result")) |>
    transmute(
      list_number = str_pad(as.character(list_number), 2, "left", "0"),
      elected = str_detect(as.character(result), regex("^gew", ignore_case = TRUE))
    ) |>
    filter(elected, !is.na(list_number)) |>
    count(list_number, name = "seats_dp") |>
    right_join(party_lookup, by = "list_number") |>
    transmute(
      canton = "GR",
      year = as.integer(election_year),
      party,
      seats_dp = replace_na(as.integer(seats_dp), 0L)
    )

  # The number of elected candidates in a district equals its seat magnitude.
  district_seats <- candidates_raw[-c(1:3), c(1, 9)] |>
    set_names(c("district", "result")) |>
    transmute(
      district = str_squish(as.character(district)),
      elected = str_detect(as.character(result), regex("^gew", ignore_case = TRUE))
    ) |>
    filter(elected, !is.na(district)) |>
    count(district, name = "district_seats")

  # The canton summary starts after the district blocks and therefore marks
  # the upper boundary of the district-level observations.
  summary_header_row <- which(
    str_detect(
      str_squish(as.character(results_raw[[2]])),
      "^Kanton Graub"
    )
  )
  stopifnot(length(summary_header_row) == 1L)

  # Carry each "Wahlkreis" name down to its Total row, then retain only those
  # district totals and the party-vote columns used in the HB counterfactual.
  district_results <- results_raw[seq_len(summary_header_row - 1L), ] |>
    mutate(
      district = if_else(
        str_detect(as.character(.data$x2), "^Wahlkreis ") &
          is.na(.data$x1),
        str_remove(as.character(.data$x2), "^Wahlkreis "),
        NA_character_
      )
    ) |>
    fill(district) |>
    filter(str_squish(as.character(.data$x2)) == "Total") |>
    select(district, all_of(party_vote_names))

  names(district_results)[-1] <- parties

  # Reshape to one district-party row and attach each district's seat count.
  hb_by_district <- district_results |>
    pivot_longer(
      cols = all_of(parties),
      names_to = "parties",
      values_to = "party_votes"
    ) |>
    mutate(party_votes = replace_na(as.numeric(party_votes), 0)) |>
    left_join(district_seats, by = "district")

  # Validate coverage and the 120-seat size of the Graubünden parliament.
  stopifnot(
    nrow(district_results) == nrow(district_seats),
    !anyNA(hb_by_district$district_seats),
    sum(district_seats$district_seats) == 120L,
    sum(official_results$seats_dp) == 120L
  )

  # Apply Hagenbach-Bischoff within each district, then aggregate party seats
  # across districts to obtain the canton-level counterfactual allocation.
  hb_summary <- hb_by_district |>
    group_by(district) |>
    mutate(
      seats_hb = allocate_hb(party_votes, first(district_seats))
    ) |>
    ungroup() |>
    group_by(parties) |>
    summarise(seats_hb = sum(seats_hb), .groups = "drop")

  # Locate the canton-total row that contains the reported vote shares.
  canton_total_row <- which(
    str_detect(
      str_squish(as.character(results_raw[[2]])),
      "^Total Kanton Graub"
    )
  )
  stopifnot(length(canton_total_row) == 1L)

  # Read each share from the column immediately right of its vote-total column.
  vote_shares <- tibble(
    parties = parties,
    vote_share = as.numeric(unlist(
      results_raw[canton_total_row, party_share_cols],
      use.names = FALSE
    ))
  )

  hb_results <- vote_shares |>
    left_join(hb_summary, by = "parties")

  # Combine vote shares, counterfactual HB seats, and official DP seats in the
  # common schema used by all canton-processing scripts.
  final <- finalize_election_data(
    hb_results,
    official_results,
    "GR",
    election_year
  )

  # Confirm that no party was lost and both allocations distribute all seats.
  stopifnot(
    nrow(final) == length(parties),
    sum(final$seats_hb) == 120L,
    sum(final$seats_dp) == 120L
  )

  final
}

process_canton_GR <- function() {
  # Process both available elections and return one stacked canton dataset.
  gr_22_final <- read_gr_election("GR_22_pressedaten.xlsx", 2022)
  gr_26_final <- read_gr_election("GR_26_pressedaten.xlsx", 2026)

  bind_rows(gr_22_final, gr_26_final)
}

# Temporary check — delete in the end.
print(process_canton_GR(), n = Inf)
