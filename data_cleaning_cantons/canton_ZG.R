# Zug-specific data cleaning for the 2014, 2018, and 2022 elections.
# Each election is supplied as one candidate-results CSV containing all
# municipalities, lists, candidates, votes, and official mandates.

# Load only the packages used directly in this script.
library(tidyverse)
library(here)

here::i_am("data_cleaning_cantons/canton_ZG.R") # Set a stable project root.

# Load the shared allocation and output-format helpers when this script runs
# on its own rather than after 00_functions.R.
if (!exists("allocate_hb", mode = "function") ||
    !exists("finalize_election_data", mode = "function")) {
  source(here("data_cleaning_cantons", "00_functions.R")) # Load shared helpers if needed.
}

# Load the Zug election data from the official CSV exports. 
# (Kanton Zug, 2022a, 2022b, 2022c, 2026)
zg_source_dir <- here("data", "datasets", "zug") # Folder containing the raw files.
zg_source_files <- tribble( # Match each election year to its source file.
  ~year, ~file_name,
  2014L, "ZG_14_kandidierendenresultate.csv",
  2018L, "ZG_18_kandidierendenresultate.csv",
  2022L, "ZG_22_kandidierendenresultate.csv",
  2026L, "ZG_26_kandidierendenresultate.csv"
)

as_zg_number <- function(value) {
  suppressWarnings(as.numeric(as.character(value))) # Convert imported text to numbers.
}

standardize_zg_party <- function(party_label) {
  normalized <- party_label |>
    as.character() |> # Ensure labels are text.
    str_replace_all("[\r\n]+", " ") |> # Remove line breaks.
    str_squish() |> # Remove repeated whitespace.
    str_to_upper() |> # Ignore capitalization differences.
    stringi::stri_trans_general("Latin-ASCII") # Replace umlauts and accents.

  case_when(
    normalized %in% c("ALTERNATIVE", "ALG") ~ "ALG", # Merge ALG labels.
    normalized %in% c("GRUNLIBERALE", "GLP") ~ "GLP", # Merge GLP labels.
    normalized %in% c("DIE MITTE", "MITTE") ~ "DIEMITTE", # Use one Mitte label.
    normalized == "AUBU" ~ "AUBUE", # Restore the transliterated party code.
    TRUE ~ normalized |>
      str_replace_all("[^A-Z0-9]+", "_") |> # Replace punctuation with underscores.
      str_remove("^_+") |>
      str_remove("_+$") # Remove leading and trailing underscores.
  )
}

# The counterfactual applies Hagenbach-Bischoff to all municipalities to
# isolate the mechanical effect of the allocation method. Historically,
# two-seat Neuheim used Majorz. An exact Majorz counterfactual cannot be
# reconstructed because hypothetical second-round results are unavailable
# (Kanton Zug, 2006, §§ 46–47, 55–56; Kanton Zug, 2022a, 2022b, 2022c).
# HB seats are first calculated from each municipality's party votes and
# seat magnitude, then summed by party to obtain canton-wide seat totals.
summarize_zg_hb <- function(district_results) {
  district_results |>
    group_by(district) |> # Allocate seats separately in each municipality.
    mutate(
      seats_hb = allocate_hb(party_votes, first(district_seats)) # calling the shared allocation helper.
    ) |>
    ungroup() |>
    group_by(parties) |> # Combine municipal seats by party.
    summarise(seats_hb = sum(seats_hb), .groups = "drop")
}

read_zg_election <- function(source_dir, file_name, election_year) {
  source_path <- file.path(source_dir, file_name) # Build the complete file path.
  stopifnot(file.exists(source_path)) # Stop if the source file is missing.

  raw <- read_delim(
    source_path,
    delim = ";", # The official export is semicolon-separated.
    col_types = cols(.default = col_character()), # Preserve the raw values as text.
    locale = locale(encoding = "UTF-8"), # Read German characters correctly.
    trim_ws = TRUE,
    show_col_types = FALSE
  )

  required_columns <- c(
    "compound_id", "compound_mandates", "election_id",
    "election_mandates", "entity_name", "entity_accounted_votes",
    "list_name", "list_id", "list_number_of_mandates", "list_votes",
    "candidate_id", "candidate_elected"
  )
  stopifnot(all(required_columns %in% names(raw))) # Confirm the expected file structure.

  # The file registry is authoritative, while these metadata checks guard
  # against assigning an export to the wrong election or parliament size.
  stopifnot(
    all(str_detect(raw$compound_id, as.character(election_year))), # Check the election year.
    n_distinct(raw$compound_mandates) == 1L, # Require one parliament size.
    as_zg_number(first(raw$compound_mandates)) == 80 # Zug has 80 seats.
  )

  elected_flag <- str_to_upper(str_squish(raw$candidate_elected)) # Standardize election flags.
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
    group_by(entity_name, list_name) |> # Inspect each municipality-party block.
    summarise(
      list_id_values = n_distinct(list_id), # Number of list IDs in the block.
      vote_values = n_distinct(list_votes), # Number of reported vote totals.
      magnitude_values = n_distinct(election_mandates), # Number of seat totals.
      accounted_values = n_distinct(entity_accounted_votes), # Number of valid-vote totals.
      mandate_values = n_distinct(list_number_of_mandates), # Number of mandate totals.
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
    group_by(entity_name, list_name) |> # Collapse repeated candidate rows.
    summarise(
      district_seats = as.integer(as_zg_number(first(election_mandates))), # Municipal seats.
      accounted_votes = as_zg_number(first(entity_accounted_votes)), # Valid municipal votes.
      party_votes = as_zg_number(first(list_votes)), # Party votes in the municipality.
      reported_seats_dp = as.integer(
        as_zg_number(first(list_number_of_mandates))
      ), # Official Double-Pukelsheim seats.
      .groups = "drop"
    ) |>
    transmute(
      district = str_squish(entity_name), # Clean the municipality name.
      district_seats,
      accounted_votes,
      parties = standardize_zg_party(list_name), # Harmonize party labels.
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
    group_by(district) |> # Validate municipality-level totals.
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

  # The district and municipialities vote shares, have to be translated to
  # canton-wide vote shares. (Kanton Zug, 2022a, 2022b, 2022c, 2026)
 
  vote_shares <- district_results |>
    mutate(
      voter_number = floor(party_votes / district_seats + 0.5) # Convert list votes to voter numbers.
    ) |>
    group_by(parties) |> # Aggregate voter numbers across municipalities.
    summarise(voter_number = sum(voter_number), .groups = "drop") |>
    mutate(vote_share = 100 * voter_number / sum(voter_number)) |> # Calculate canton-wide shares.
    select(parties, vote_share)

  elected_results <- tibble(
    parties = standardize_zg_party(raw$list_name), # Match elected candidates to parties.
    elected = elected_flag == "TRUE" # Convert the flag to logical values.
  ) |>
    filter(elected) |> # Retain elected candidates only.
    count(parties, name = "seats_dp") # Count official seats by party.

  official_results <- district_results |>
    distinct(parties) |> # Keep every contesting party.
    left_join(elected_results, by = "parties") |> # Add official seat totals.
    transmute(
      canton = "ZG",
      year = as.integer(election_year),
      party = parties,
      seats_dp = replace_na(as.integer(seats_dp), 0L) # Assign zero seats where none were won.
    )

  # The candidate election flags and the repeated list-mandate field are two
  # independent representations of the official Double-Pukelsheim result.
  reported_results <- district_results |>
    group_by(parties) |> # Sum the second official seat measure.
    summarise(
      seats_dp_reported = sum(reported_seats_dp),
      .groups = "drop"
    )
  dp_check <- official_results |>
    left_join(reported_results, by = c("party" = "parties")) # Compare both seat measures.
  stopifnot(
    !anyNA(dp_check$seats_dp_reported),
    all(dp_check$seats_dp == dp_check$seats_dp_reported)
  )

  final <- finalize_election_data( # Create the common canton-year output format.
    vote_shares |>
      left_join(summarize_zg_hb(district_results), by = "parties"),
    official_results,
    "ZG",
    election_year
  )

  stopifnot(
    nrow(final) == n_distinct(district_results$parties),
    sum(as.integer(final$seats_hb)) == 80L,
    sum(final$seats_dp) == 80L
  )

  final
}

process_canton_ZG <- function() {
  map2_dfr(
    zg_source_files$file_name,
    zg_source_files$year,
    ~ read_zg_election(zg_source_dir, .x, .y) # Process and combine all three elections.
  )
}

# Temporary check - delete in the end.
options(width = 200)
print(process_canton_ZG(), n = 200)
