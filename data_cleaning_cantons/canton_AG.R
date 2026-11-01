# Aargau-specific data cleaning and processing
# Processes all Aargau elections (2009, 2012, 2016, 2020, 2024) and returns a unified dataset.
# 
# This script extracts Aargau data from raw Excel files,
# applies Hagenbach-Bischoff allocation, and merges with official results.
#
# Dependencies: 00_functions.R (allocate_hb, finalize_election_data)

# Load required libraries
library(tidyverse)
library(readxl)
library(here)


# set path
here::i_am("data_cleaning_cantons/canton_AG.R")

# Load shared helpers when this file runs on its own.
if (!exists("allocate_hb", mode = "function") ||
    !exists("finalize_election_data", mode = "function")) {
  source(here("data_cleaning_cantons", "00_functions.R"))
}

read_ag_cantonal_seats <- function(file_name, election_year, election_parties) {
  official_raw <- read_excel(
    here("data", "datasets", "aargau", file_name), # Build a project-independent path to the election file.
    sheet = "Data",                                # Read the worksheet containing the official results.
    na = c("", "...")                             # Treat empty cells and "..." as NAs.
  )

  # Rename and match name of the parties from seats_cantonal_level files
  names(official_raw)[7:20] <- c(
    "total", "svp", "sp", "cvp", "fdp", "gruene", "evp", "glp",
    "bdp", "edu", "sd", "fps", "ldu", "uebrige"
  )

  official_row <- official_raw |>
    # Keep the single cantonal-level row for the requested election year.
    filter(year == election_year, locationType == "CANTON")

 
  stopifnot(
    nrow(official_row) == 1L, # stop if not exactly one row
    as.integer(official_row$total) == 140L # stop if not exactly 140 seats
  )

  reported_seats <- official_row |>
    select(svp:uebrige) |> # Retain only the columns containing party seat totals.
    pivot_longer(
      cols = everything(),       # Reshape every selected party column into a row.
      names_to = "party",       # Store the former column name as the party identifier.
      values_to = "seats_dp"    # Store  official Double-Pukelsheim seats as a value.
    ) |>
    transmute(
      party = str_to_upper(party),                       # Match the uppercase party codes used later for joining.
      seats_dp = replace_na(as.integer(seats_dp), 0L)   # Convert seats to integers and missing entries to zero.
    )

  # Start from the parties contesting this election so that each party receives one result row.
  tibble(party = str_to_upper(election_parties)) |>
    left_join(reported_seats, by = "party") |> # Attach the officially reported seats by party code.
    mutate(
      canton = "AG",                                  # Add the canton identifier required by the combined dataset.
      year = as.integer(election_year),                # Add the election year as an integer.
      seats_dp = replace_na(seats_dp, 0L)              # Give unreported contesting parties zero seats.
    ) |>
    select(canton, year, party, seats_dp) # Return only the standardized columns needed for the later join.
}

read_ag_district_seats_and_results <- function(
  file_name,
  election_year,
  election_parties,
  result_party_names
) {
  allocation_raw <- read_excel(
    here("data", "datasets", "aargau", file_name),
    sheet = "Data",
    na = c("", "...")
  ) |>
    janitor::clean_names()

  # The first result block contains the district magnitude in its TOTAL column
  # and the official Double-Pukelsheim party allocations in the following columns.
  names(allocation_raw)[7:18] <- c("district_seats", result_party_names)

  district_seats <- allocation_raw |>
    filter(year == election_year, location_type == "DISTRICT") |>
    transmute(
      location_name,
      district_seats = as.integer(district_seats)
    )

  reported_seats <- allocation_raw |>
    filter(year == election_year, location_type == "CANTON") |>
    select(all_of(result_party_names)) |>
    pivot_longer(
      cols = everything(),
      names_to = "party",
      values_to = "seats_dp"
    ) |>
    transmute(
      party = str_to_upper(party),
      seats_dp = replace_na(as.integer(seats_dp), 0L)
    )

  official_results <- tibble(party = str_to_upper(election_parties)) |>
    left_join(reported_seats, by = "party") |>
    mutate(
      canton = "AG",
      year = as.integer(election_year),
      seats_dp = replace_na(seats_dp, 0L)
    ) |>
    select(canton, year, party, seats_dp)

  stopifnot(
    nrow(district_seats) == 11L,
    !anyNA(district_seats$district_seats),
    sum(district_seats$district_seats) == 140L,
    sum(official_results$seats_dp) == 140L
  )

  list(
    district_seats = district_seats,
    official_results = official_results
  )
}

# Apply the former Aargau Hagenbach-Bischoff method district by district
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
      seats_hb = allocate_hb(party_votes, first(district_seats)),
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

# Read and process one election using its row in ag_source_files.
read_ag_election <- function(
  election_year, vote_file, reference_file, result_party_names
) {
  raw <- read_excel(
    here("data", "datasets", "aargau", vote_file),
    sheet = "Data"
  ) |>
    janitor::clean_names()

  # The vote workbooks share these administrative columns in every election.
  non_party_columns <- c(
    "bfs_nr", "year", "month", "day", "location_name", "location_type", "total"
  )
  parties <- setdiff(names(raw), non_party_columns)

  if (is.null(result_party_names)) {
    # The 2009/2012/2016 references report only the official cantonal seats.
    official_results <- read_ag_cantonal_seats(
      reference_file, election_year, parties
    )
    district_seats <- ag_district_seats_2009_2016
  } else {
    # The 2020/2024 references also supply the district magnitudes.
    reference <- read_ag_district_seats_and_results(
      reference_file, election_year, parties, result_party_names
    )
    official_results <- reference$official_results
    district_seats <- reference$district_seats
  }

  districts <- raw |>
    filter(location_type == "DISTRICT") |>
    left_join(district_seats, by = "location_name") |>
    mutate(
      across(
        c(total, all_of(parties)),
        ~ replace_na(as.numeric(na_if(as.character(.x), "...")), 0)
      )
    )

  stopifnot(
    nrow(districts) == 11L,
    !anyDuplicated(districts$location_name),
    !anyNA(districts$district_seats),
    sum(districts$district_seats) == 140L
  )

  finalize_ag_election(districts, parties, official_results, election_year)
}

# Process every registered election and combine the standardized results.
process_canton_AG <- function() {
  purrr::pmap_dfr(ag_source_files, read_ag_election)
}

# Temporary check - delete in the end
options(width = 200) 
print(process_canton_AG(), n = 200)
