# Aargau-specific data cleaning and processing
# Processes all Aargau elections (2009, 2012, 2015, 2019, 2023) and returns a unified dataset.
# 
# This script extracts Aargau data from raw Excel files,
# applies Hagenbach-Bischoff allocation, and merges with official results.
#
# Dependencies: 00_functions.R (allocate_hb, finalize_election_data)
#               Libraries: tidyverse, readxl, janitor

library(tidyverse)
library(readxl)
library(janitor)
library(here)

read_ag_cantonal_seats <- function(file_name, election_year, election_parties) {
  official_raw <- read_excel(
    here("data", "datasets", "aargau", file_name), # Build a project-independent path to the election file.
    sheet = "Data",                                # Read the worksheet containing the official results.
    na = c("", "...")                             # Treat empty cells and "..." as missing values.
  )

  # Give the party-result columns consistent names across the official files.
  names(official_raw)[7:20] <- c(
    "total", "svp", "sp", "cvp", "fdp", "gruene", "evp", "glp",
    "bdp", "edu", "sd", "fps", "ldu", "uebrige"
  )

  official_row <- official_raw |>
    # Keep the single cantonal-level row for the requested election year.
    filter(year == election_year, locationType == "CANTON")

  # Stop immediately if the expected row is missing/duplicated or does not report 140 seats.
  stopifnot(
    nrow(official_row) == 1L,
    as.integer(official_row$total) == 140L
  )

  reported_seats <- official_row |>
    select(svp:uebrige) |> # Retain only the columns containing party seat totals.
    pivot_longer(
      cols = everything(),       # Reshape every selected party column into a row.
      names_to = "party",       # Store the former column name as the party identifier.
      values_to = "seats_dp"    # Store the official Double-Pukelsheim seats as a value.
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
  clean_names()

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
  # "..." entries (party did not stand) are converted to zero
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
  
  # Apply Hagenbach-Bischoff allocation separately per district
  # This implements the 2005 Aargau Grossratswahlgesetz (Sections 12-13)
  # Assumes no list alliances (as per 2009 law)
  ag_09_hb_by_district <- ag_09_districts |>
    select(location_name, district_seats, all_of(ag_09_parties)) |>
    pivot_longer(
      cols = all_of(ag_09_parties), # Turn the separate party columns into party-level rows.
      names_to = "parties",        # Store each party code in a common identifier column.
      values_to = "party_votes"    # Store its district vote total in a common value column.
    ) |>
    group_by(location_name) |> # Run the seat allocation independently within each district.
    mutate(
      seats_hb = allocate_hb(party_votes, first(district_seats)), # Allocate the district's seats using Hagenbach-Bischoff.
      voter_number = floor(
        party_votes / first(district_seats) + 0.5
      ) # Convert party votes into the comparable cantonal voter-number measure.
    ) |>
    ungroup()
  
  # Aggregate to cantonal level and prepare final dataset
  ag_09_hb_summary <- ag_09_hb_by_district |>
    group_by(parties) |> # Combine each party's results across all Aargau districts.
    summarise(
      voter_number = sum(voter_number), # Sum the party's voter numbers across districts.
      seats_hb = sum(seats_hb),         # Sum the party's district seats across districts.
      .groups = "drop"                  # Return an ungrouped summary table.
    ) |>
    mutate(
      parties = str_to_upper(parties),                       # Match the official-results party-code format.
      vote_share = 100 * voter_number / sum(voter_number)    # Calculate each party's cantonal vote share in percent.
    ) |>
    select(parties, vote_share, seats_hb)
  
  # Finalize 2009 data
  ag_09_final <- finalize_election_data(
    ag_09_hb_summary,
    ag_09_official_results,
    "AG",
    2009
  )
  
  # Validation
  stopifnot(
    nrow(ag_09_final) == length(ag_09_parties),
    sum(ag_09_final$seats_hb) == 140L,
    sum(ag_09_final$seats_dp) == 140L
  )

  # ===========================================================================
  # 2012 Election
  # ===========================================================================

  # Import raw Aargau 2012 dataset
  # (Statistisches Amt Kanton Aargau, n.d.)
  ag_12_raw <- read_excel(
    here("data", "datasets", "aargau", "AG_13_kantonsratswahlen.xlsx"),
    sheet = "Data"
  )

  # Parties correspond to the party columns of the raw Aargau 2012 dataset.
  ag_12_parties <- c(
    "svp", "sp", "cvp", "fdp", "gruene", "evp", "sd",
    "edu", "bdp", "slb", "glp", "pp", "igg"
  )

  # Official Double Pukelsheim results for Aargau 2012
  # (Regierungsrat des Kantons Aargau, 2012)
  ag_12_official_results <- read_ag_cantonal_seats(
    "AG_12_seats_cantonal_level.xlsx", # File containing the official cantonal seat totals.
    2012,                               # Select the 2012 cantonal result row.
    ag_12_parties                       # Ensure every party contesting in 2012 appears in the output.
  )

  # District seat magnitudes for 2012
  # Source: Staatskanzlei des Kantons Aargau (2008)
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
    clean_names() |> # Standardize the imported Excel column names to snake_case.
    filter(location_type == "DISTRICT") |> # Exclude canton-level and other non-district rows.
    mutate(
      district_seats = unname(ag_12_district_seats[location_name]), # Match each district to its seat magnitude.
      across(
        c(total, all_of(ag_12_parties)),
        ~ replace_na(as.numeric(na_if(as.character(.x), "...")), 0) # Convert vote fields to numbers and non-participation to zero.
      )
    )

  # Apply Hagenbach-Bischoff allocation separately per district
  # This implements the 2005 Aargau Grossratswahlgesetz (Sections 12-13)
  # Assumes no list alliances (as per 2009 law)
  ag_12_hb_by_district <- ag_12_districts |>
    select(location_name, district_seats, all_of(ag_12_parties)) |>
    pivot_longer(
      cols = all_of(ag_12_parties), # Turn the separate party columns into party-level rows.
      names_to = "parties",        # Store each party code in a common identifier column.
      values_to = "party_votes"    # Store its district vote total in a common value column.
    ) |>
    group_by(location_name) |> # Run the seat allocation independently within each district.
    mutate(
      seats_hb = allocate_hb(party_votes, first(district_seats)), # Allocate the district's seats using Hagenbach-Bischoff.
      voter_number = floor(
        party_votes / first(district_seats) + 0.5
      ) # Convert party votes into the comparable cantonal voter-number measure.
    ) |>
    ungroup()

  # Aggregate to cantonal level and prepare final dataset
  ag_12_hb_summary <- ag_12_hb_by_district |>
    group_by(parties) |> # Combine each party's results across all Aargau districts.
    summarise(
      voter_number = sum(voter_number), # Sum the party's voter numbers across districts.
      seats_hb = sum(seats_hb),         # Sum the party's district seats across districts.
      .groups = "drop"                  # Return an ungrouped summary table.
    ) |>
    mutate(
      parties = str_to_upper(parties),                       # Match the official-results party-code format.
      vote_share = 100 * voter_number / sum(voter_number)    # Calculate each party's cantonal vote share in percent.
    ) |>
    select(parties, vote_share, seats_hb)

  # Finalize 2012 data
  ag_12_final <- finalize_election_data(
    ag_12_hb_summary,
    ag_12_official_results,
    "AG",
    2012
  )

  # Validation
  stopifnot(
    nrow(ag_12_final) == length(ag_12_parties),
    sum(ag_12_final$seats_hb) == 140L,
    sum(ag_12_final$seats_dp) == 140L
  )

  # ===========================================================================
  # Combine all Aargau elections
  # ===========================================================================

  ag_final <- bind_rows(
    ag_09_final,
    ag_12_final
  ) # Stack the standardized election results into one Aargau dataset.

  return(ag_final) # Make the combined Aargau dataset available to the calling script.
}
