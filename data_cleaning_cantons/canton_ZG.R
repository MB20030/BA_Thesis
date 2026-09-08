# Zug-specific data cleaning for the 2014, 2018, and 2022 elections.
# Each election is supplied as one candidate-results CSV containing all
# municipalities, lists, candidates, votes, and official mandates.

# Load only the packages used directly in this script.
library(tidyverse)
library(here)

here::i_am("data_cleaning_cantons/canton_ZG.R")

# Load the shared allocation and output-format helpers when this script runs
# on its own rather than after 00_functions.R.
if (!exists("allocate_hb", mode = "function") ||
    !exists("finalize_election_data", mode = "function")) {
  source(here("data_cleaning_cantons", "00_functions.R"))
}

# SOURCE FILES: one complete candidate-results export per election.
# add SOURCE HERE----------------------------------------------------------
zg_source_dir <- here("data", "datasets", "zug")
zg_source_files <- tribble(
  ~year, ~file_name,
  2014L, "ZG_14_kandidierendenresultate.csv",
  2018L, "ZG_18_kandidierendenresultate.csv",
  2022L, "ZG_22_kandidierendenresultate.csv"
)

as_zg_number <- function(value) {
  suppressWarnings(as.numeric(as.character(value)))
}

standardize_zg_party <- function(party_label) {
  normalized <- party_label |>
    as.character() |>
    str_replace_all("[\r\n]+", " ") |>
    str_squish() |>
    str_to_upper() |>
    stringi::stri_trans_general("Latin-ASCII")

  case_when(
    normalized %in% c("ALTERNATIVE", "ALG") ~ "ALG",
    normalized %in% c("GRUNLIBERALE", "GLP") ~ "GLP",
    normalized == "DIE MITTE" ~ "DIEMITTE",
    normalized == "AUBU" ~ "AUBUE",
    TRUE ~ normalized |>
      str_replace_all("[^A-Z0-9]+", "_") |>
      str_remove("^_+") |>
      str_remove("_+$")
  )
}

summarize_zg_hb <- function(district_results) {
  district_results |>
    group_by(district) |>
    mutate(
      seats_hb = allocate_hb(party_votes, first(district_seats))
    ) |>
    ungroup() |>
    group_by(parties) |>
    summarise(seats_hb = sum(seats_hb), .groups = "drop")
}

read_zg_election <- function(source_dir, file_name, election_year) {
  source_path <- file.path(source_dir, file_name)
  stopifnot(file.exists(source_path))

  raw <- read_delim(
    source_path,
    delim = ";",
    col_types = cols(.default = col_character()),
    locale = locale(encoding = "UTF-8"),
    trim_ws = TRUE,
    show_col_types = FALSE
  )

  required_columns <- c(
    "compound_id", "compound_mandates", "election_id",
    "election_mandates", "entity_name", "entity_accounted_votes",
    "list_name", "list_id", "list_number_of_mandates", "list_votes",
    "candidate_id", "candidate_elected"
  )
  stopifnot(all(required_columns %in% names(raw)))

  # The file registry is authoritative, while these metadata checks guard
  # against assigning an export to the wrong election or parliament size.
  stopifnot(
    all(str_detect(raw$compound_id, as.character(election_year))),
    n_distinct(raw$compound_mandates) == 1L,
    as_zg_number(first(raw$compound_mandates)) == 80
  )

  elected_flag <- str_to_upper(str_squish(raw$candidate_elected))
  stopifnot(
    !anyNA(raw$entity_name),
    !anyNA(raw$list_name),
    !anyNA(raw$candidate_id),
    !anyNA(elected_flag),
    all(elected_flag %in% c("TRUE", "FALSE"))
  )

  # Candidate rows repeat the municipality and list totals. Confirm that
  # every municipality-list block contains one internally consistent value
  # before retaining a single copy of those totals.
  source_structure <- raw |>
    group_by(entity_name, list_name) |>
    summarise(
      list_id_values = n_distinct(list_id),
      vote_values = n_distinct(list_votes),
      magnitude_values = n_distinct(election_mandates),
      accounted_values = n_distinct(entity_accounted_votes),
      mandate_values = n_distinct(list_number_of_mandates),
      .groups = "drop"
    )
  stopifnot(
    all(source_structure$list_id_values == 1L),
    all(source_structure$vote_values == 1L),
    all(source_structure$magnitude_values == 1L),
    all(source_structure$accounted_values == 1L),
    all(source_structure$mandate_values == 1L)
  )

  district_results <- raw |>
    group_by(entity_name, list_name) |>
    summarise(
      district_seats = as.integer(as_zg_number(first(election_mandates))),
      accounted_votes = as_zg_number(first(entity_accounted_votes)),
      party_votes = as_zg_number(first(list_votes)),
      reported_seats_dp = as.integer(
        as_zg_number(first(list_number_of_mandates))
      ),
      .groups = "drop"
    ) |>
    transmute(
      district = str_squish(entity_name),
      district_seats,
      accounted_votes,
      parties = standardize_zg_party(list_name),
      party_votes,
      reported_seats_dp
    )

  stopifnot(
    !anyNA(district_results),
    all(district_results$district_seats > 0L),
    all(district_results$party_votes >= 0),
    all(district_results$reported_seats_dp >= 0L),
    !anyDuplicated(district_results[c("district", "parties")])
  )

  district_checks <- district_results |>
    group_by(district) |>
    summarise(
      magnitude_values = n_distinct(district_seats),
      accounted_values = n_distinct(accounted_votes),
      district_seats = first(district_seats),
      accounted_votes = first(accounted_votes),
      party_votes = sum(party_votes),
      reported_seats_dp = sum(reported_seats_dp),
      .groups = "drop"
    )
  stopifnot(
    nrow(district_checks) == 11L,
    all(district_checks$magnitude_values == 1L),
    all(district_checks$accounted_values == 1L),
    all(district_checks$party_votes == district_checks$accounted_votes),
    all(district_checks$reported_seats_dp == district_checks$district_seats),
    sum(district_checks$district_seats) == 80L
  )

  # Zug municipalities have different council magnitudes. Convert each
  # municipality's list votes to the comparable voter-number measure before
  # aggregating canton-wide vote shares.
  vote_shares <- district_results |>
    mutate(
      voter_number = floor(party_votes / district_seats + 0.5)
    ) |>
    group_by(parties) |>
    summarise(voter_number = sum(voter_number), .groups = "drop") |>
    mutate(vote_share = 100 * voter_number / sum(voter_number)) |>
    select(parties, vote_share)

  elected_results <- tibble(
    parties = standardize_zg_party(raw$list_name),
    elected = elected_flag == "TRUE"
  ) |>
    filter(elected) |>
    count(parties, name = "seats_dp")

  official_results <- district_results |>
    distinct(parties) |>
    left_join(elected_results, by = "parties") |>
    transmute(
      canton = "ZG",
      year = as.integer(election_year),
      party = parties,
      seats_dp = replace_na(as.integer(seats_dp), 0L)
    )

  # The candidate election flags and the repeated list-mandate field are two
  # independent representations of the official Double-Pukelsheim result.
  reported_results <- district_results |>
    group_by(parties) |>
    summarise(
      seats_dp_reported = sum(reported_seats_dp),
      .groups = "drop"
    )
  dp_check <- official_results |>
    left_join(reported_results, by = c("party" = "parties"))
  stopifnot(
    !anyNA(dp_check$seats_dp_reported),
    all(dp_check$seats_dp == dp_check$seats_dp_reported)
  )

  final <- finalize_election_data(
    vote_shares |>
      left_join(summarize_zg_hb(district_results), by = "parties"),
    official_results,
    "ZG",
    election_year
  )

  stopifnot(
    nrow(final) == n_distinct(district_results$parties),
    sum(final$seats_hb) == 80L,
    sum(final$seats_dp) == 80L
  )

  final
}

process_canton_ZG <- function() {
  map2_dfr(
    zg_source_files$file_name,
    zg_source_files$year,
    ~ read_zg_election(zg_source_dir, .x, .y)
  )
}

# Temporary check - delete in the end.
print(process_canton_ZG(), n = Inf)
