# Schwyz-specific data cleaning for the 2016, 2020, and 2024 elections.

# Load only the packages used directly in this script.
library(tidyverse)
library(readxl)
library(here)

here::i_am("data_cleaning_cantons/canton_SZ.R") # Fix the project root when this script runs alone.

if (!exists("allocate_hb", mode = "function") ||
    !exists("allocate_hb_with_notes", mode = "function") ||
    !exists("finalize_election_data", mode = "function")) {
  source(here("data_cleaning_cantons", "00_functions.R")) # Load the shared allocation and output helpers.
}

if (!exists('combine_hb_seat_options', mode = 'function')) {
  source(here('data_cleaning_cantons', '00_functions.R'))
}

# SOURCE ADD HERE: 
sz_source_dir <- here("data", "datasets", "schwyz") # Common folder for every Schwyz source file.
sz_source_files <- list(
  election_2016 = paste0(                                  # One SZ_16_municipality CSV per municipality.
    "SZ_16_",
    c(
      "alpthal", "altendorf", "arth", "einsiedeln", "feusisberg", "freienbach",
      "galgenen", "gersau", "illgau", "ingenbohl", "innerthal", "k\u00fcssnacht",
      "lachen", "lauerz", "morschach", "moutathal", "oberiberg", "reichenburg",
      "riemenstalden", "rothenturm", "sattel", "schwyz", "sch\u00fcbelbach", "steinen",
      "steinerberg", "tuggen", "unteriberg", "vorderthal", "wangen", "wollerau"
    ),
    ".csv"
  ),
  election_2020 = "SZ_20_kandidierendenresultate.csv",   # Candidate and municipality results for 2020.
  election_2024 = "SZ_24_kantonsprotokoll.xlsx"          # Cantonal protocol with Sitzverteilung for 2024.
)

as_sz_number <- function(value) {
  suppressWarnings(as.numeric(as.character(value))) # Convert mixed spreadsheet cells without noisy header warnings.
}

standardize_sz_party <- function(party_label) {
  normalized <- party_label |>
    as.character() |>
    str_replace_all("[\r\n]+", " ") |>
    str_squish() |>
    str_to_upper() |>
    stringi::stri_trans_general("Latin-ASCII") # Make party identifiers stable across encodings and years.

  case_when(
    normalized %in% c("SP-GR-UA", "SP, G U U") ~ "SP_GRUENE_UN", # The 2016/2020 joint SP-Greens-independent list.
    normalized == "SP" ~ "SP",                              # In 2024 the list is SP alone.
    normalized == "DIE MITTE" ~ "DIEMITTE",                         # Standardize the post-CVP party label.
    normalized == "EVP - EDU" ~ "EVP_EDU",                          # Keep the 2024 joint list together.
    normalized %in% c("WOP", "WAHLZETTEL OHNE PARTEIBEZEICHNUNG") ~ NA_character_, # Exclude no-party ballots.
    TRUE ~ normalized |>
      str_replace_all("[^A-Z0-9]+", "_") |>
      str_remove("^_+") |>
      str_remove("_+$") # Convert any remaining label to the common identifier format.
  )
}

# Compute HB seat allocation based on the former law 
# (Bundesgericht, 2012, E. 5.2)
summarize_sz_hb <- function(district_results) {
  district_results |>
    group_by(district) |> # Recreate the former system municipality by municipality.
    group_modify(function(data, key) {
      allocation <- allocate_hb_with_notes(
        data$party_votes,
        first(data$district_seats),
        data$parties
      )

      bind_cols(data, allocation) |>
        mutate(
          hb_tie_note = if_else(
            is.na(hb_tie_note),
            NA_character_,
            paste0("Municipality ", key$district, ": ", hb_tie_note)
          )
        )
    }) |> # Allocate each municipality's seats by HB and retain unresolved ties.
    ungroup() |>
    group_by(parties) |> # Combine the municipal HB results at cantonal level.
    summarise(
      seats_hb_options = combine_hb_seat_options(seats_hb_options),
      seats_hb = unique_hb_seat_value(seats_hb_options),
      hb_tie_note = {
        notes <- unique(na.omit(hb_tie_note))
        if (length(notes) == 0L) NA_character_ else paste(notes, collapse = " | ")
      },
      .groups = "drop"
    )
}

read_sz_2016 <- function(source_dir, file_names) {
  source_files <- file.path(source_dir, file_names) # Build the 30 required municipality paths explicitly.
  stopifnot(length(source_files) == 30L, all(file.exists(source_files))) # Stop if any required 2016 export is missing.

  raw <- map_dfr(source_files, function(file_path) {
    read_delim(
      file_path,                                          # Read one municipality's semicolon-delimited export.
      delim = ";",
      col_types = cols(.default = col_character()),      # Preserve identifiers and convert numbers explicitly below.
      locale = locale(encoding = "UTF-8"),
      trim_ws = TRUE,
      show_col_types = FALSE
    ) |>
      mutate(
        district = basename(file_path) |>
          tools::file_path_sans_ext() |>
          str_remove(regex("^SZ_16_", ignore_case = TRUE)) # Derive the municipality from its required file name.
      )
  })

  required_columns <- c("ListCode", "StimmenTotal", "StimmenProz", "Sitze") # Columns C, E, G, and H in the source.
  stopifnot(all(required_columns %in% names(raw))) # Stop if a municipality export changes structure.

  district_results <- raw |>
    transmute(
      district,
      parties = standardize_sz_party(ListCode),
      party_votes = as_sz_number(StimmenTotal),           # Column E is aggregated; column G is deliberately not averaged.
      seats_dp = replace_na(as.integer(as_sz_number(Sitze)), 0L) # Column H contains the official municipal DP seats.
    ) |>
    filter(!is.na(parties)) |>                            # Remove only the no-party ballot category (WoP).
    group_by(district) |>
    mutate(district_seats = sum(seats_dp)) |>             # The municipal seat total is the historical district magnitude.
    ungroup()

  vote_shares <- district_results |>
    group_by(parties) |>
    summarise(party_votes = sum(party_votes), .groups = "drop") |> # Sum votes across all municipalities before dividing.
    mutate(vote_share = 100 * party_votes / sum(party_votes)) |>    # Use the canton-wide party-vote denominator.
    select(parties, vote_share)

  official_results <- district_results |>
    group_by(parties) |>
    summarise(seats_dp = sum(seats_dp), .groups = "drop") |> # Sum the official municipality allocations by party.
    transmute(canton = "SZ", year = 2016L, party = parties, seats_dp)

  final <- finalize_election_data(
    vote_shares |> left_join(summarize_sz_hb(district_results), by = "parties"), # Join vote shares to HB seats.
    official_results,
    "SZ",
    2016
  )

  district_totals <- district_results |> distinct(district, district_seats) # Count every district magnitude once.
  stopifnot(
    nrow(district_totals) == 30L,
    sum(district_totals$district_seats) == 100L,
    nrow(final) == n_distinct(district_results$parties),
    any(grepl('/', final$seats_hb, fixed = TRUE)) ||
      sum(as.integer(final$seats_hb)) == 100L,
    sum(final$seats_dp) == 100L
  )

  final
}

read_sz_2020 <- function(source_dir, file_name) {
  raw <- read_delim(
    file.path(source_dir, file_name),                     # Read the requested 2020 candidate-results export.
    delim = ";",
    col_types = cols(.default = col_character()),        # Keep the import deterministic across machines.
    locale = locale(encoding = "UTF-8"),
    trim_ws = TRUE,
    show_col_types = FALSE
  )

  required_columns <- c(
    "entity_name", "election_mandates", "entity_accounted_votes",
    "list_name", "list_number_of_mandates", "list_votes", "candidate_elected"
  )
  stopifnot(all(required_columns %in% names(raw))) # Stop if columns X, AO, AP, AS, AT, or AZ are unavailable.

  source_structure <- raw |>
    group_by(entity_name, list_name) |>
    summarise(
      vote_values = n_distinct(list_votes),               # Each repeated candidate row must report the same list votes.
      magnitude_values = n_distinct(election_mandates),   # Each list must repeat one municipality magnitude.
      accounted_values = n_distinct(entity_accounted_votes),
      mandate_values = n_distinct(list_number_of_mandates),
      .groups = "drop"
    )
  stopifnot(all(source_structure[c("vote_values", "magnitude_values", "accounted_values", "mandate_values")] == 1L))

  district_results <- raw |>
    group_by(entity_name, list_name) |>
    summarise(
      district_seats = as.integer(as_sz_number(first(election_mandates))), # Column X is the municipality magnitude.
      accounted_votes = as_sz_number(first(entity_accounted_votes)),      # Column AO is the municipality-wide vote total.
      party_votes = as_sz_number(first(list_votes)),                      # Column AT is the party-specific list-vote total.
      reported_seats_dp = as.integer(as_sz_number(first(list_number_of_mandates))), # Column AS is a DP cross-check.
      .groups = "drop"
    ) |>
    transmute(
      district = entity_name,
      district_seats,
      accounted_votes,
      parties = standardize_sz_party(list_name),          # Column AP identifies the party/list.
      party_votes,
      reported_seats_dp
    )

  district_checks <- district_results |>
    group_by(district) |>
    summarise(
      magnitude_values = n_distinct(district_seats),
      accounted_values = n_distinct(accounted_votes),
      district_seats = first(district_seats),
      accounted_votes = first(accounted_votes),
      party_votes = sum(party_votes),
      .groups = "drop"
    )
  stopifnot(
    all(district_checks$magnitude_values == 1L),
    all(district_checks$accounted_values == 1L),
    all(district_checks$party_votes == district_checks$accounted_votes) # Reconcile AT party totals to AO in every municipality.
  )

  total_accounted_votes <- sum(district_checks$accounted_votes) # Count column AO once per municipality.
  vote_shares <- district_results |>
    group_by(parties) |>
    summarise(party_votes = sum(party_votes), .groups = "drop") |> # Add each party's municipal list votes canton-wide.
    mutate(vote_share = 100 * party_votes / total_accounted_votes) |> # Divide by all accounted votes in the election.
    select(parties, vote_share)

  elected_results <- raw |>
    transmute(
      parties = standardize_sz_party(list_name),
      elected = str_to_upper(str_squish(candidate_elected)) == "TRUE" # Column AZ marks elected candidates.
    ) |>
    filter(elected) |>
    count(parties, name = "seats_dp")

  official_results <- district_results |>
    distinct(parties) |>
    left_join(elected_results, by = "parties") |>
    transmute(
      canton = "SZ",
      year = 2020L,
      party = parties,
      seats_dp = replace_na(as.integer(seats_dp), 0L)
    )

  reported_results <- district_results |>
    group_by(parties) |>
    summarise(seats_dp_reported = sum(reported_seats_dp), .groups = "drop") # Independently total column AS.
  dp_check <- official_results |>
    left_join(reported_results, by = c("party" = "parties")) # Compare candidate counts with reported list mandates.

  final <- finalize_election_data(
    vote_shares |> left_join(summarize_sz_hb(district_results), by = "parties"), # Run HB within each municipality.
    official_results,
    "SZ",
    2020
  )

  stopifnot(
    nrow(district_checks) == 30L,
    sum(district_checks$district_seats) == 100L,
    sum(district_results$party_votes) == total_accounted_votes,
    all(dp_check$seats_dp == dp_check$seats_dp_reported),
    nrow(final) == n_distinct(district_results$parties),
    any(grepl('/', final$seats_hb, fixed = TRUE)) ||
      sum(as.integer(final$seats_hb)) == 100L,
    sum(final$seats_dp) == 100L
  )

  final
}

read_sz_2024 <- function(source_dir, file_name) {
  raw <- read_excel(
    file.path(source_dir, file_name),                     # Read the requested 2024 cantonal protocol.
    sheet = "Sitzverteilung",
    col_names = FALSE,
    na = c("", "..."),
    .name_repair = ~ paste0("x", seq_along(.x))
  )

  party_rows <- 9:14                                     # Rows 9:14 contain the six cantonal list groups.
  parties <- standardize_sz_party(raw[[2]][party_rows])  # Column B contains the party/list names.
  vote_shares <- tibble(
    parties,
    vote_share = as_sz_number(raw[[4]][party_rows])      # Column D contains the official cantonal percentages.
  )
  upper_results <- tibble(
    party = parties,
    seats_dp_upper = as.integer(as_sz_number(raw[[7]][party_rows])) # Column G gives the cantonal upper allocation.
  )

  block_first_rows <- c(9L, 22L, 35L, 51L, 64L, 77L, 93L, 106L) # First party row in each municipality block.
  district_vote_columns <- c(8L, 11L, 14L, 17L)                  # H, K, N, and Q contain party votes.

  district_results <- map_dfr(block_first_rows, function(first_party_row) {
    map_dfr(district_vote_columns, function(vote_column) {
      seat_label <- as.character(raw[[vote_column]][first_party_row - 2L]) # Seat magnitude sits two rows above party data.
      if (is.na(seat_label) || !str_detect(seat_label, regex("Sitz", ignore_case = TRUE))) return(tibble()) # Skip empty slots.

      block_parties <- standardize_sz_party(raw[[2]][first_party_row:(first_party_row + 5L)]) # Re-read the block labels.
      stopifnot(identical(parties, block_parties)) # Every block must preserve the six cantonal list groups.

      tibble(
        district = raw[[vote_column]][first_party_row - 3L] |> as.character() |> str_squish(), # Municipality header.
        district_seats = as.integer(str_extract(seat_label, "\\d+")), # Parse values such as "9 Sitze".
        parties = block_parties,
        party_votes = replace_na(as_sz_number(raw[[vote_column]][first_party_row:(first_party_row + 5L)]), 0), # H/K/N/Q.
        seats_dp = replace_na(as.integer(as_sz_number(raw[[vote_column + 2L]][first_party_row:(first_party_row + 5L)])), 0L) # J/M/P/S.
      )
    })
  })

  official_results <- district_results |>
    group_by(parties) |>
    summarise(seats_dp = sum(seats_dp), .groups = "drop") |> # Sum the lower allocations across municipalities.
    transmute(canton = "SZ", year = 2024L, party = parties, seats_dp)

  dp_check <- official_results |>
    left_join(upper_results, by = "party") # Reconcile J/M/P/S totals with the cantonal allocation in G.

  final <- finalize_election_data(
    vote_shares |> left_join(summarize_sz_hb(district_results), by = "parties"), # Apply HB to H/K/N/Q municipality votes.
    official_results,
    "SZ",
    2024
  )

  district_totals <- district_results |> distinct(district, district_seats) # Count every municipality once.
  stopifnot(
    !anyDuplicated(parties),
    nrow(district_totals) == 30L,
    sum(district_totals$district_seats) == 100L,
    all(dp_check$seats_dp == dp_check$seats_dp_upper),
    nrow(final) == length(parties),
    any(grepl('/', final$seats_hb, fixed = TRUE)) ||
      sum(as.integer(final$seats_hb)) == 100L,
    sum(final$seats_dp) == 100L
  )

  final
}

process_canton_SZ <- function() {
  sz_16_final <- read_sz_2016(sz_source_dir, sz_source_files$election_2016)         # Use all SZ_16 municipality files.
  sz_20_final <- read_sz_2020(sz_source_dir, sz_source_files$election_2020)         # Use SZ_20_kandidierendenresultate.
  sz_24_final <- read_sz_2024(sz_source_dir, sz_source_files$election_2024)         # Use SZ_24_kantonsprotokoll.

  bind_rows(sz_16_final, sz_20_final, sz_24_final) # Return the common election-party schema for all three years.
}

# Temporary check - delete in the end.
options(width = 200)
print(process_canton_SZ(), n = Inf, width = 200)
