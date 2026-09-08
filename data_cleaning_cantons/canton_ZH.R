# Zurich-specific data cleaning for the 2007, 2011, 2015, 2019, and 2023
# cantonal elections. Each election has separate party- and candidate-results
# workbooks.

library(tidyverse)
library(here)

here::i_am("data_cleaning_cantons/canton_ZH.R") # Set a stable project root.

# Load the shared allocation and output helpers when this script runs alone.
if (!exists("allocate_hb", mode = "function") ||
    !exists("finalize_election_data", mode = "function")) {
  source(here("data_cleaning_cantons", "00_functions.R"))
}

zh_source_dir <- here("data", "datasets", "zurich")
zh_source_files <- tribble(
  ~year, ~party_file, ~candidate_file,
  2007L, "ZH_07_parteiresultate.xlsx", "ZH_07_kandidierendenresultate.xlsx",
  2011L, "ZH_11_parteiresultate.xlsx", "ZH_11_kandidierendenresultate.xlsx",
  2015L, "ZH_15_parteiresultate.xlsx", "ZH_15_kandidierendenresultate.xlsx",
  2019L, "ZH_19_parteiresultate.xlsx", "ZH_19_kandidierendenresultate.xlsx",
  2023L, "ZH_23_parteiresultate.xlsx", "ZH_23_kandidierendenresultate.xlsx"
)

as_zh_number <- function(value) {
  suppressWarnings(as.numeric(as.character(value)))
}

normalize_zh_label <- function(label) {
  label |>
    as.character() |>
    str_replace_all("[\r\n]+", " ") |>
    str_squish() |>
    str_to_upper() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_replace_all("[^A-Z0-9]+", " ") |>
    str_squish()
}

standardize_zh_party <- function(party_label) {
  normalized <- normalize_zh_label(party_label)

  case_when(
    normalized %in% c("GP", "GRUNE") ~ "GRUENE",
    normalized %in% c("MITTE", "DIE MITTE") ~ "DIEMITTE",
    normalized %in% c("FGA AL PDA", "AL", "PDA") ~ "FGA_AL_PDA",
    normalized %in% c(
      "UBRIGE", "HANF UELI", "PIRATEN", "ERP", "IP ZH", "JUSO",
      "DIE GUTEN", "HELVIDA", "AUFL", "SAPAPO", "STOPP"
    ) ~ "UEBRIGE",
    TRUE ~ str_replace_all(normalized, " ", "_")
  )
}

read_zh_election <- function(
  source_dir,
  party_file,
  candidate_file,
  election_year
) {
  party_path <- file.path(source_dir, party_file)
  candidate_path <- file.path(source_dir, candidate_file)
  stopifnot(file.exists(party_path), file.exists(candidate_path))

  party_raw <- readxl::read_excel(party_path, col_types = "text")
  candidate_raw <- readxl::read_excel(candidate_path, col_types = "text")

  # Guard the fixed source columns supplied by the canton: B/H/K and A/C/L.
  stopifnot(
    identical(
      names(party_raw)[c(2, 8, 11)],
      c("Geschaeft", "Partei", "Partei_ParteistimmenTotal")
    ),
    identical(
      names(candidate_raw)[c(1, 3, 12)],
      c("Geschaeft", "Liste_Code", "Kand_Gewaehlt")
    )
  )

  party_observations <- party_raw |>
    transmute(
      district = str_squish(Geschaeft),                      # Column B.
      unit_id = str_squish(Einheit_Code),
      unit_vote_total = as_zh_number(Einheit_StimmenTotal),
      parties = standardize_zh_party(Partei),                # Column H.
      party_votes_raw = as_zh_number(Partei_ParteistimmenTotal) # Column K.
    ) |>
    mutate(
      # The 2011 export doubles every SP total in column K. Halving those
      # values restores the reported total for every municipality.
      party_votes = if_else(
        election_year == 2011L & parties == "SP",
        party_votes_raw / 2,
        party_votes_raw
      )
    )

  stopifnot(
    !anyNA(party_observations),
    all(str_detect(party_observations$district, as.character(election_year))),
    all(party_observations$party_votes >= 0),
    all(party_observations$party_votes == floor(party_observations$party_votes)),
    !anyDuplicated(party_observations[c("district", "unit_id", "parties")])
  )

  unit_checks <- party_observations |>
    group_by(district, unit_id) |>
    summarise(
      total_values = n_distinct(unit_vote_total),
      unit_vote_total = first(unit_vote_total),
      party_votes = sum(party_votes),
      .groups = "drop"
    )
  stopifnot(
    all(unit_checks$total_values == 1L),
    all(unit_checks$unit_vote_total == unit_checks$party_votes)
  )

  # Column B identifies the voting district. Sum municipality results before
  # applying the historical district-level allocation.
  district_results <- party_observations |>
    group_by(district, parties) |>
    summarise(party_votes = sum(party_votes), .groups = "drop")

  elected_code <- as_zh_number(candidate_raw$Kand_Gewaehlt)
  candidate_results <- candidate_raw |>
    transmute(
      district = str_squish(Geschaeft),
      candidate_id = str_squish(Liste_KandID),
      parties = standardize_zh_party(Liste_Code),
      elected_code,
      elected = elected_code == 1
    )

  stopifnot(
    !anyNA(candidate_results),
    all(str_detect(candidate_results$district, as.character(election_year))),
    all(candidate_results$elected_code %in% c(0, 1)),
    !anyDuplicated(candidate_results[c("district", "candidate_id")])
  )

  # Every mapped candidate list must correspond to one party group in the
  # party-results workbook, including FGA/AL/PdA and the miscellaneous lists.
  party_coverage <- district_results |>
    distinct(district, parties)
  candidate_coverage <- candidate_results |>
    distinct(district, parties)
  stopifnot(
    nrow(anti_join(party_coverage, candidate_coverage,
      by = c("district", "parties")
    )) == 0L,
    nrow(anti_join(candidate_coverage, party_coverage,
      by = c("district", "parties")
    )) == 0L
  )

  # Counting elected candidates in each district reconstructs its magnitude;
  # counting them by party gives the official Double-Pukelsheim result.
  district_seats <- candidate_results |>
    group_by(district) |>
    summarise(district_seats = sum(elected), .groups = "drop")

  elected_results <- candidate_results |>
    filter(elected) |>
    count(parties, name = "seats_dp")

  official_results <- district_results |>
    distinct(parties) |>
    left_join(elected_results, by = "parties") |>
    transmute(
      canton = "ZH",
      year = as.integer(election_year),
      party = parties,
      seats_dp = replace_na(as.integer(seats_dp), 0L)
    )

  hb_by_district <- district_results |>
    left_join(district_seats, by = "district") |>
    group_by(district) |>
    mutate(
      seats_hb = allocate_hb(party_votes, first(district_seats)),
      voter_number = floor(party_votes / first(district_seats) + 0.5)
    ) |>
    ungroup()

  hb_results <- hb_by_district |>
    group_by(parties) |>
    summarise(
      voter_number = sum(voter_number),
      seats_hb = sum(seats_hb),
      .groups = "drop"
    ) |>
    mutate(vote_share = 100 * voter_number / sum(voter_number)) |>
    select(parties, vote_share, seats_hb)

  final <- finalize_election_data(
    hb_results,
    official_results,
    "ZH",
    election_year
  )

  district_checks <- hb_by_district |>
    group_by(district) |>
    summarise(
      district_seats = first(district_seats),
      seats_hb = sum(seats_hb),
      .groups = "drop"
    )

  stopifnot(
    nrow(district_checks) == 18L,
    !anyNA(district_checks$district_seats),
    all(district_checks$district_seats > 0L),
    all(district_checks$seats_hb == district_checks$district_seats),
    sum(district_checks$district_seats) == 180L,
    nrow(final) == n_distinct(district_results$parties),
    sum(final$seats_hb) == 180L,
    sum(final$seats_dp) == 180L
  )

  final
}

process_canton_ZH <- function() {
  purrr::pmap_dfr(
    zh_source_files,
    function(year, party_file, candidate_file) {
      read_zh_election(
        zh_source_dir,
        party_file,
        candidate_file,
        year
      )
    }
  )
}

# Temporary check - delete in the end.
print(process_canton_ZH(), n = Inf)
