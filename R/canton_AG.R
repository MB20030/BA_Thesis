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
    here("data", "datasets", "aargau", file_name),
    sheet = "Data",
    na = c("", "...")
  )

  names(official_raw)[7:20] <- c(
    "total", "svp", "sp", "cvp", "fdp", "gruene", "evp", "glp",
    "bdp", "edu", "sd", "fps", "ldu", "uebrige"
  )

  official_row <- official_raw |>
    filter(year == election_year, locationType == "CANTON")

  stopifnot(
    nrow(official_row) == 1L,
    as.integer(official_row$total) == 140L
  )

  reported_seats <- official_row |>
    select(svp:uebrige) |>
    pivot_longer(
      cols = everything(),
      names_to = "party",
      values_to = "seats_dp"
    ) |>
    transmute(
      party = str_to_upper(party),
      seats_dp = replace_na(as.integer(seats_dp), 0L)
    )

  tibble(party = str_to_upper(election_parties)) |>
    left_join(reported_seats, by = "party") |>
    mutate(
      canton = "AG",
      year = as.integer(election_year),
      seats_dp = replace_na(seats_dp, 0L)
    ) |>
    select(canton, year, party, seats_dp)
}

process_canton_AG <- function() {

  # ===========================================================================
  # 2009 Election
  # ===========================================================================

  # Party list for Aargau 2009
  ag_09_parties <- c(
    "svp", "sp", "cvp", "fdp", "gruene", "evp", "sd",
    "edu", "bdp", "uns", "glp", "npak", "l_plus", "faag"
  )

  # Official Double Pukelsheim results for Aargau 2009
  # (Department für Finanzen und Ressourcen des Kantons Aargau, 2009)
  ag_09_official_results <- read_ag_cantonal_seats(
    "AG_09_seats_cantonal_level.xlsx",
    2009,
    ag_09_parties
  )
  
  # Import raw Aargau 2009 dataset
  # (Statistisches Amt Kanton Aargau, n.d.)
  ag_09_raw <- read_excel(
    here("data", "datasets", "aargau", "AG_09_kantonsratswahlen.xlsx"),
    sheet = "Data"
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
    clean_names() |>
    filter(location_type == "DISTRICT") |>
    mutate(
      district_seats = unname(ag_09_district_seats[location_name]),
      across(
        c(total, all_of(ag_09_parties)),
        ~ replace_na(as.numeric(na_if(as.character(.x), "...")), 0)
      )
    )
  
  # Apply Hagenbach-Bischoff allocation separately per district
  # This implements the 2005 Aargau Grossratswahlgesetz (Sections 12-13)
  # Assumes no list alliances (as per 2009 law)
  ag_09_hb_by_district <- ag_09_districts |>
    select(location_name, district_seats, all_of(ag_09_parties)) |>
    pivot_longer(
      cols = all_of(ag_09_parties),
      names_to = "parties",
      values_to = "party_votes"
    ) |>
    group_by(location_name) |>
    mutate(
      seats_hb = allocate_hb(party_votes, first(district_seats)),
      voter_number = floor(
        party_votes / first(district_seats) + 0.5
      )
    ) |>
    ungroup()
  
  # Aggregate to cantonal level and prepare final dataset
  ag_09_hb_summary <- ag_09_hb_by_district |>
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

  # Party list for Aargau 2012
  ag_12_parties <- c(
    "svp", "sp", "cvp", "fdp", "gruene", "evp", "sd",
    "edu", "bdp", "slb", "glp", "pp", "igg"
  )

  # Official Double Pukelsheim results for Aargau 2012
  # (Regierungsrat des Kantons Aargau, 2012)
  ag_12_official_results <- read_ag_cantonal_seats(
    "AG_12_seats_cantonal_level.xlsx",
    2012,
    ag_12_parties
  )

  # Import raw Aargau 2012 dataset
  # (Statistisches Amt Kanton Aargau, n.d.)
  ag_12_raw <- read_excel(
    here("data", "datasets", "aargau", "AG_13_kantonsratswahlen.xlsx"),
    sheet = "Data"
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
    clean_names() |>
    filter(location_type == "DISTRICT") |>
    mutate(
      district_seats = unname(ag_12_district_seats[location_name]),
      across(
        c(total, all_of(ag_12_parties)),
        ~ replace_na(as.numeric(na_if(as.character(.x), "...")), 0)
      )
    )

  # Apply Hagenbach-Bischoff allocation separately per district
  # This implements the 2005 Aargau Grossratswahlgesetz (Sections 12-13)
  # Assumes no list alliances (as per 2009 law)
  ag_12_hb_by_district <- ag_12_districts |>
    select(location_name, district_seats, all_of(ag_12_parties)) |>
    pivot_longer(
      cols = all_of(ag_12_parties),
      names_to = "parties",
      values_to = "party_votes"
    ) |>
    group_by(location_name) |>
    mutate(
      seats_hb = allocate_hb(party_votes, first(district_seats)),
      voter_number = floor(
        party_votes / first(district_seats) + 0.5
      )
    ) |>
    ungroup()

  # Aggregate to cantonal level and prepare final dataset
  ag_12_hb_summary <- ag_12_hb_by_district |>
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
  )

  return(ag_final)
}
