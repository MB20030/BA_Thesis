# Aargau-specific data cleaning and processing
# Processes all Aargau elections (2009, 2015, etc.)
# 
# This script extracts Aargau data from raw Excel files,
# applies Hagenbach-Bischoff allocation, and merges with official results.
#
# Dependencies: 00_functions.R (allocate_hb, finalize_election_data)
#               Libraries: tidyverse, readxl, janitor

process_canton_AG <- function() {
  
  # Official DP results for Aargau
  # Source: Department für Finanzen und Ressourcen des Kantons Aargau (2009)
  official_results <- tribble(
    ~canton, ~year, ~party, ~seats_dp,
    # Aargau 2009
    "AG", 2009, "SVP", 45L,
    "AG", 2009, "SP", 22L,
    "AG", 2009, "CVP", 21L,
    "AG", 2009, "FDP", 20L,
    "AG", 2009, "GRUENE", 13L,
    "AG", 2009, "EVP", 6L,
    "AG", 2009, "SD", 2L,
    "AG", 2009, "EDU", 2L,
    "AG", 2009, "BDP", 4L,
    "AG", 2009, "UNS", 0L,
    "AG", 2009, "GLP", 5L,
    "AG", 2009, "NPAK", 0L,
    "AG", 2009, "L_PLUS", 0L,
    "AG", 2009, "FAAG", 0L
  )
  
  # ===========================================================================
  # 2009 Election
  # ===========================================================================
  
  # Import raw Aargau 2009 dataset
  # Source: Staatskanzlei des Kantons Aargau (2008)
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
  
  # Party list for Aargau 2009
  ag_09_parties <- c(
    "svp", "sp", "cvp", "fdp", "gruene", "evp", "sd",
    "edu", "bdp", "uns", "glp", "npak", "l_plus", "faag"
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
  ag_09_final <- finalize_election_data(ag_09_hb_summary, official_results, "AG", 2009)
  
  # Validation
  stopifnot(
    nrow(ag_09_final) == length(ag_09_parties),
    sum(ag_09_final$seats_hb) == 140L,
    sum(ag_09_final$seats_dp) == 140L
  )
  
  # ===========================================================================
  # Combine all Aargau elections
  # ===========================================================================
  
  ag_final <- bind_rows(ag_09_final)
  
  return(ag_final)
}
