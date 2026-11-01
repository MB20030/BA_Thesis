# Aargau-specific data cleaning for the 2009, 2012, 2016, 2020, and 2024 elections.
# Returns one row per election and party, comparing district-level HB allocations
# with the observed official Double-Pukelsheim seats.
# Shared helpers: 00_functions.R (allocate_hb, finalize_election_data).
# The source files differ, therefore two reference readers are needed:
# 2009/2012/2016 references contain cantonal party seats while district magnitudes
# are entered in the table below. The 2020/2024 references contain both.

library(tidyverse)
library(readxl)
library(here)

here::i_am("data_cleaning_cantons/canton_AG.R") # Set the project root

# Load shared helpers when this file runs on its own
if (!exists("allocate_hb", mode = "function") ||
    !exists("finalize_election_data", mode = "function")) {
  source(here("data_cleaning_cantons", "00_functions.R"))
}

# Convert vote and seat counts to numeric, interpreting "..." as zero.
as_ag_number <- function(value) {
  text <- str_trim(as.character(value))
  text[text %in% "..."] <- "0"
  suppressWarnings(as.numeric(text))
}

# Read the official party seats from the 2009/2012/2016 cantonal seat references
read_ag_cantonal_seats <- function(file_name, election_year, election_parties) {
  official_raw <- read_excel(
    here("data", "datasets", "aargau", file_name),
    sheet = "Data",
    na = ""
  )

  # Remove the seat-block prefix and clean column names
  names(official_raw) <- str_remove(names(official_raw), "^SIS_GRWA0003D€1€")
  official_raw <- official_raw |>
    janitor::clean_names() |>
    rename(cvp = diemitte) # Keep the historical CVP label for these elections

  party_columns <- c(
    "svp", "sp", "cvp", "fdp", "gruene", "evp", "glp",
    "bdp", "edu", "sd", "fps", "ldu", "uebrige"
  )
  stopifnot(all(c("year", "location_type", "total", party_columns) %in% names(official_raw)))

  official_row <- official_raw |>
    filter(year == election_year, location_type == "CANTON")
  stopifnot(nrow(official_row) == 1L, as_ag_number(official_row$total) == 140L)

  reported_seats <- official_row |> # Extract the official party seats
    select(all_of(party_columns)) |>
    pivot_longer(
      cols = everything(),
      names_to = "party",
      values_to = "seats_dp",
      values_transform = list(seats_dp = as_ag_number)
    ) |>
    mutate(party = str_to_upper(party))

  # Check the original counts before any missing join matches are filled
  stopifnot(
    all(is.finite(reported_seats$seats_dp)),
    all(reported_seats$seats_dp >= 0),
    all(reported_seats$seats_dp == floor(reported_seats$seats_dp)),
    sum(reported_seats$seats_dp) == 140L
  )

  tibble(party = str_to_upper(election_parties)) |>
    left_join(reported_seats, by = "party") |>
    mutate(
      canton = "AG",
      year = as.integer(election_year),
      # Replace any remaining parties that received no official seats with zeroes
      seats_dp = replace_na(as.integer(seats_dp), 0L)
    ) |>
    select(canton, year, party, seats_dp)
}

# Read district magnitudes and official party seats from the 2020/2024 references
read_ag_district_seats_and_results <- function(
  file_name, election_year, election_parties, result_party_names
) {
  allocation_raw <- read_excel(
    here("data", "datasets", "aargau", file_name),
    sheet = "Data",
    na = ""
  )

  # Extract the district magnitudes and official party seats
  names(allocation_raw) <- str_remove(names(allocation_raw), "^SIS_GRWA0006D€2€")
  allocation_raw <- allocation_raw |>
    janitor::clean_names() |>
    rename(district_seats = total)
  if ("cvp" %in% result_party_names) {
    allocation_raw <- allocation_raw |> rename(cvp = diemitte)
  }
  stopifnot(all(
    c("year", "location_type", "location_name", result_party_names) %in% names(allocation_raw)
  ))
  # Extract and validate district magnitudes and official party seats
  district_seats <- allocation_raw |>
    filter(year == election_year, location_type == "DISTRICT") |>
    transmute(location_name, district_seats = as_ag_number(district_seats))

  official_row <- allocation_raw |>
    filter(year == election_year, location_type == "CANTON")
  stopifnot(
    nrow(official_row) == 1L,
    as_ag_number(official_row$district_seats) == 140L,
    nrow(district_seats) == 11L,
    !anyNA(district_seats$location_name),
    !anyDuplicated(district_seats$location_name),
    all(is.finite(district_seats$district_seats)),
    all(district_seats$district_seats > 0),
    all(district_seats$district_seats == floor(district_seats$district_seats)),
    sum(district_seats$district_seats) == 140L
  )

  reported_seats <- official_row |>
    select(all_of(result_party_names)) |>
    pivot_longer(
      cols = everything(),
      names_to = "party",
      values_to = "seats_dp",
      values_transform = list(seats_dp = as_ag_number)
    ) |>
    mutate(party = str_to_upper(party))
  stopifnot(
    all(is.finite(reported_seats$seats_dp)),
    all(reported_seats$seats_dp >= 0),
    all(reported_seats$seats_dp == floor(reported_seats$seats_dp)),
    sum(reported_seats$seats_dp) == 140L
  )
 # Match official seats to contesting parties, fill omitted zero-seat parties
  official_results <- tibble(party = str_to_upper(election_parties)) |>
    left_join(reported_seats, by = "party") |>
    mutate(
      canton = "AG",
      year = as.integer(election_year),
      seats_dp = replace_na(as.integer(seats_dp), 0L)
    ) |>
    select(canton, year, party, seats_dp)
  stopifnot(sum(official_results$seats_dp) == 140L)

  list(district_seats = district_seats, official_results = official_results)
}

# Apply the former Aargau Hagenbach-Bischoff method 
# (Kanton Aargau, 1988/2004, § 12 and § 13(1)(a–b))
finalize_ag_election <- function(districts, parties, official_results, election_year) {
  hb_by_district <- districts |>
    select(location_name, district_seats, all_of(parties)) |>
    pivot_longer(
      cols = all_of(parties),
      names_to = "parties",
      values_to = "party_votes"
    ) |>
    group_by(location_name) |>
    mutate(
      seats_hb = allocate_hb(party_votes, first(district_seats)), # Call the HB allocator 
      # Normalize for district magnitude and round halves upward per district
      voter_number = floor(party_votes / first(district_seats) + 0.5)
    ) |>
    ungroup()

  hb_summary <- hb_by_district |>
    group_by(parties) |>
    summarise(
      voter_number = sum(voter_number),
      seats_hb = sum(seats_hb),
      .groups = "drop"
    ) |>
    mutate(
      parties = str_to_upper(parties),
      # Aggregate the district-rounded voter equivalents into cantonal shares
      vote_share = 100 * voter_number / sum(voter_number)
    ) |>
    select(parties, vote_share, seats_hb)

  final <- finalize_election_data(
    hb_summary,
    official_results,
    "AG",
    election_year
  )

  stopifnot(
    nrow(final) == length(parties),
    # The shared schema stores HB seats as text; AG requires a unique allocation.
    sum(as.integer(final$seats_hb)) == 140L,
    sum(final$seats_dp) == 140L
  )

  final
}

# Raw vote workbooks for all five elections:
# (Statistisches Amt Kanton Aargau, n.d.)
# Official Double Pukelsheim results:
# 2009: (Department für Finanzen und Ressourcen des Kantons Aargau, 2009)
# 2012: (Regierungsrat des Kantons Aargau, 2013)
# 2016 cantonal results and 2020/2024 district magnitudes and seat allocations:
# (Statistisches Amt Kanton Aargau, n.d.)
# NULL means that the reference workbook contains cantonal seat totals only.
# Otherwise, result_party_names gives the party order in the allocation workbook.
ag_source_files <- tribble(
  ~election_year, ~vote_file, ~reference_file, ~result_party_names,
  2009L, "AG_09_kantonsratswahlen.xlsx", "AG_09_seats_cantonal_level.xlsx", NULL,
  2012L, "AG_12_kantonsratswahlen.xlsx", "AG_12_seats_cantonal_level.xlsx", NULL,
  2016L, "AG_16_kantonsratswahlen.xlsx", "AG_16_seats_cantonal_level.xlsx", NULL,
  2020L, "AG_20_kantonsratswahlen.xlsx", "AG_20_district_magnitudes_seat_allocation.xlsx",
    c("svp", "fdp", "sp", "cvp", "gruene", "glp", "evp", "edu", "lovb", "fff", "ppag"),
  2024L, "AG_24_kantonsratswahlen.xlsx", "AG_24_district_magnitudes_seat_allocation.xlsx",
    c("svp", "fdp", "sp", "diemitte", "gruene", "glp", "evp", "edu", "lovb", "fff", "ppag")
)

# The district magnitudes are identical in 2009, 2012, and 2016.
# 2009 source: Staatskanzlei des Kantons Aargau (2008)
# 2012: (Staatskanzlei des Kantons Aargau, 2012)
# 2016: (Staatskanzlei des Kantons Aargau, 2016)
ag_district_seats_2009_2016 <- tribble(
  ~location_name, ~district_seats,
  "Bezirk Aarau", 16L,
  "Bezirk Baden", 30L,
  "Bezirk Bremgarten", 16L,
  "Bezirk Brugg", 11L,
  "Bezirk Kulm", 9L,
  "Bezirk Laufenburg", 7L,
  "Bezirk Lenzburg", 12L,
  "Bezirk Muri", 7L,
  "Bezirk Rheinfelden", 10L,
  "Bezirk Zofingen", 15L,
  "Bezirk Zurzach", 7L
)

# Read one configured election and return the standardized party-level results
read_ag_election <- function(
  election_year, vote_file, reference_file, result_party_names
) {
  raw <- read_excel(
    here("data", "datasets", "aargau", vote_file),
    sheet = "Data",
    na = ""
  ) |>
    janitor::clean_names()

  non_party_columns <- c(
    "bfs_nr", "year", "month", "day", "location_name", "location_type", "total"
  )
  stopifnot(
    all(non_party_columns %in% names(raw)),
    all(raw$year == election_year)
  )
  parties <- setdiff(names(raw), non_party_columns)
  stopifnot(length(parties) > 0L)

  if (is.null(result_party_names)) {
    # Earlier references report cantonal seats, district magnitudes are listed above
    official_results <- read_ag_cantonal_seats(
      reference_file, election_year, parties
    )
    district_seats <- ag_district_seats_2009_2016
  } else {
    reference <- read_ag_district_seats_and_results(
      reference_file, election_year, parties, result_party_names
    )
    official_results <- reference$official_results
    district_seats <- reference$district_seats
  }

  districts <- raw |>
    filter(year == election_year, location_type == "DISTRICT") |>
    left_join(district_seats, by = "location_name") |>
    mutate(across(c(total, all_of(parties)), as_ag_number))

  # Check district identities, matched magnitudes, and vote totals 
  vote_counts <- as.matrix(districts[c("total", parties)])
  stopifnot(
    nrow(districts) == 11L,
    !anyNA(districts$location_name),
    !anyDuplicated(districts$location_name),
    !anyNA(districts$district_seats),
    all(districts$district_seats > 0),
    sum(districts$district_seats) == 140L,
    all(is.finite(vote_counts)),
    all(vote_counts >= 0),
    all(districts$total == rowSums(as.matrix(districts[parties])))
  )

  finalize_ag_election(districts, parties, official_results, election_year)
}

# Process all registered elections and combine their standardized results.
process_canton_AG <- function() {
  purrr::pmap_dfr(ag_source_files, read_ag_election)
}

