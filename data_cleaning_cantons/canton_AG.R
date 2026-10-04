# Aargau-specific data cleaning and processing
# Processes all Aargau elections (2009, 2012, 2016, 2020, 2024) and returns a unified dataset.
# 
# This script extracts Aargau data from raw Excel files,
# applies Hagenbach-Bischoff allocation, and merges with official results.
#
# Dependencies: 00_functions.R (allocate_hb, finalize_election_data)

# Load only the packages used directly in this script.
library(tidyverse)
library(readxl)
library(here)


# Ensure here() uses the correct project root.
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
    na = c("", "...")                             # Treat empty cells and "..." as missing values.
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

process_canton_AG <- function() {

  # ===========================================================================
  # 2009 Election
  # ===========================================================================

  # Import raw Aargau 2009 dataset
  # (Statistisches Amt Kanton Aargau, n.d.)
  # Import the raw Aargau 2009 dataset and standardize its column names.
ag_09_raw <- read_excel(
  here("data", "datasets", "aargau", "AG_09_kantonsratswahlen.xlsx"),
  sheet = "Data"
) |>
  janitor::clean_names()

# Identify the columns containing administrative information rather than parties.
non_party_columns <- c(
  "bfs_nr",
  "year",
  "month",
  "day",
  "location_name",
  "location_type",
  "total"
)

# Treat all remaining columns as party columns.
ag_09_parties <- setdiff(
  names(ag_09_raw),
  non_party_columns
)

  # Official Double Pukelsheim results for Aargau 2009
  # (Department für Finanzen und Ressourcen des Kantons Aargau, 2009)
  ag_09_official_results <- read_ag_cantonal_seats(
    "AG_09_seats_cantonal_level.xlsx", # File containing the official cantonal seat totals.
    2009,                               # Select the 2009 cantonal result row.
    ag_09_parties                       # Ensure every party contesting in 2009 appears in the output.
  )
  
  # District seat magnitudes for 2009
  # Source: Staatskanzlei des Kantons Aargau (2008)
  ag_09_district_seats <- c(
    "Bezirk Aarau" = 16L,
    "Bezirk Baden" = 30L,
    "Bezirk Bremgarten" = 16L,
    "Bezirk Brugg" = 11L,
    "Bezirk Kulm" = 9L,
    "Bezirk Laufenburg" = 7L,
    "Bezirk Lenzburg" = 12L,
    "Bezirk Muri" = 7L,
    "Bezirk Rheinfelden" = 10L,
    "Bezirk Zofingen" = 15L,
    "Bezirk Zurzach" = 7L
  )
  
  # Clean and prepare district-level data
  # "..." (empty) entries are converted to zero
 ag_09_districts <- ag_09_raw |>
  filter(location_type == "DISTRICT") |> # Keep only district-level observations.
  mutate(
    district_seats = unname(
      ag_09_district_seats[location_name]
    ),
    across(
      c(total, all_of(ag_09_parties)),
      ~ replace_na(
        as.numeric(na_if(as.character(.x), "...")),
        0
      )
    )
  )
  
  ag_09_final <- finalize_ag_election(
    ag_09_districts, ag_09_parties, ag_09_official_results, 2009
  )

  # ===========================================================================
  # 2012 Election
  # ===========================================================================

  # Import raw Aargau 2012 dataset
  # (Statistisches Amt Kanton Aargau, n.d.)
  ag_12_raw <- read_excel(
    here("data", "datasets", "aargau", "AG_12_kantonsratswahlen.xlsx"),
    sheet = "Data"
  )

  # Parties correspond to the party columns of the raw Aargau 2012 dataset.
  ag_12_parties <- c(
    "svp", "sp", "cvp", "fdp", "gruene", "evp", "sd",
    "edu", "bdp", "slb", "glp", "pp", "igg"
  )

  # Official Double Pukelsheim results for Aargau 2012
  # (Regierungsrat des Kantons Aargau, 2013)
  ag_12_official_results <- read_ag_cantonal_seats(
    "AG_12_seats_cantonal_level.xlsx", # File containing the official cantonal seat totals.
    2012,                               # Select the 2012 cantonal result row.
    ag_12_parties                       # Ensure every party contesting in 2012 appears in the output.
  )

  # District seat magnitudes for 2012 (Staatskanzlei des Kantons Aargau, 2012)
  ag_12_district_seats <- c(
    "Bezirk Aarau" = 16L,
    "Bezirk Baden" = 30L,
    "Bezirk Bremgarten" = 16L,
    "Bezirk Brugg" = 11L,
    "Bezirk Kulm" = 9L,
    "Bezirk Laufenburg" = 7L,
    "Bezirk Lenzburg" = 12L,
    "Bezirk Muri" = 7L,
    "Bezirk Rheinfelden" = 10L,
    "Bezirk Zofingen" = 15L,
    "Bezirk Zurzach" = 7L
  )

  # Clean and prepare district-level data
  # "..." entries (party did not stand) are converted to zero
  ag_12_districts <- ag_12_raw |>
    janitor::clean_names() |> # Standardize the imported Excel column names to snake_case.
    filter(location_type == "DISTRICT") |> # Exclude canton-level and other non-district rows.
    mutate(
      district_seats = unname(ag_12_district_seats[location_name]), # Match each district to its seat magnitude.
      across(
        c(total, all_of(ag_12_parties)),
        ~ replace_na(as.numeric(na_if(as.character(.x), "...")), 0) # Convert vote fields to numbers and non-participation to zero.
      )
    )

  ag_12_final <- finalize_ag_election(
    ag_12_districts, ag_12_parties, ag_12_official_results, 2012
  )

  # ===========================================================================
  # 2016 Election
  # ===========================================================================

  # Import raw Aargau 2016 dataset
  # (Statistisches Amt Kanton Aargau, n.d.)
  ag_16_raw <- read_excel(
    here("data", "datasets", "aargau", "AG_16_kantonsratswahlen.xlsx"),
    sheet = "Data"
  ) |>
    janitor::clean_names()

  ag_16_parties <- setdiff(names(ag_16_raw), non_party_columns)

  # Import raw seat allocation result 
  # (Statistisches Amt Kanton Aargau, n.d.)
  ag_16_official_results <- read_ag_cantonal_seats(
    "AG_16_seats_cantonal_level.xlsx",
    2016,
    ag_16_parties
  )

  # District seat magnitudes for 2016, (Staatskanzlei des Kantons Aargau, 2016)
  ag_16_district_seats <- c(
    "Bezirk Aarau" = 16L,
    "Bezirk Baden" = 30L,
    "Bezirk Bremgarten" = 16L,
    "Bezirk Brugg" = 11L,
    "Bezirk Kulm" = 9L,
    "Bezirk Laufenburg" = 7L,
    "Bezirk Lenzburg" = 12L,
    "Bezirk Muri" = 7L,
    "Bezirk Rheinfelden" = 10L,
    "Bezirk Zofingen" = 15L,
    "Bezirk Zurzach" = 7L
  )

  ag_16_districts <- ag_16_raw |>
    filter(location_type == "DISTRICT") |>
    mutate(
      district_seats = unname(ag_16_district_seats[location_name]),
      across(
        c(total, all_of(ag_16_parties)),
        ~ replace_na(as.numeric(na_if(as.character(.x), "...")), 0)
      )
    )

  ag_16_final <- finalize_ag_election(
    ag_16_districts, ag_16_parties, ag_16_official_results, 2016
  )

  # ===========================================================================
  # 2020 Election
  # ===========================================================================
  
  # Import raw Aargau 2020 dataset
  # (Statistisches Amt Kanton Aargau, n.d.)
  ag_20_raw <- read_excel(
    here("data", "datasets", "aargau", "AG_20_kantonsratswahlen.xlsx"),
    sheet = "Data"
  ) |>
    janitor::clean_names()

  ag_20_parties <- setdiff(names(ag_20_raw), non_party_columns)

  # Import raw district seat allocation and results dataset
  # (Statistisches Amt Kanton Aargau, n.d.)
  ag_20_reference <- read_ag_district_seats_and_results(
    "AG_20_district_magnitudes_seat_allocation.xlsx",
    2020,
    ag_20_parties,
    c("svp", "fdp", "sp", "cvp", "gruene", "glp", "evp", "edu", "lovb", "fff", "ppag")
  )

  ag_20_districts <- ag_20_raw |>
    filter(location_type == "DISTRICT") |>
    left_join(ag_20_reference$district_seats, by = "location_name") |>
    mutate(
      across(
        c(total, all_of(ag_20_parties)),
        ~ replace_na(as.numeric(na_if(as.character(.x), "...")), 0)
      )
    )

  ag_20_final <- finalize_ag_election(
    ag_20_districts, ag_20_parties, ag_20_reference$official_results, 2020
  )

  # ===========================================================================
  # 2024 Election
  # ===========================================================================

  # Import raw Aargau 2024 dataset
  # (Statistisches Amt Kanton Aargau, n.d.)
  ag_24_raw <- read_excel(
    here("data", "datasets", "aargau", "AG_24_kantonsratswahlen.xlsx"),
    sheet = "Data"
  ) |>
    janitor::clean_names()

  ag_24_parties <- setdiff(names(ag_24_raw), non_party_columns)

   # Import raw district seat allocation and results dataset
   # (Statistisches Amt Kanton Aargau, n.d.)
  ag_24_reference <- read_ag_district_seats_and_results(
    "AG_24_district_magnitudes_seat_allocation.xlsx",
    2024,
    ag_24_parties,
    c("svp", "fdp", "sp", "diemitte", "gruene", "glp", "evp", "edu", "lovb", "fff", "ppag")
  )

  ag_24_districts <- ag_24_raw |>
    filter(location_type == "DISTRICT") |>
    left_join(ag_24_reference$district_seats, by = "location_name") |>
    mutate(
      across(
        c(total, all_of(ag_24_parties)),
        ~ replace_na(as.numeric(na_if(as.character(.x), "...")), 0)
      )
    )

  ag_24_final <- finalize_ag_election(
    ag_24_districts, ag_24_parties, ag_24_reference$official_results, 2024
  )

  # ===========================================================================
  # Combine all Aargau elections
  # ===========================================================================

  ag_final <- bind_rows(
    ag_09_final,
    ag_12_final,
    ag_16_final,
    ag_20_final,
    ag_24_final
  ) # Stack the standardized election results into one Aargau dataset.

  return(ag_final) # Make the combined Aargau dataset available to the calling script.
}

# Temporary check - delete in the end.
print(process_canton_AG(), n = Inf)
