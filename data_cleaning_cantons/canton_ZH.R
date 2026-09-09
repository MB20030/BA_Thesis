# Zurich-specific data cleaning for the 2007, 2011, 2015, 2019, and 2023
# cantonal elections. Each election has separate party- and candidate-results
# workbooks.
#
# Source-data disclaimer: In ZH_11_parteiresultate.xlsx, every SP total in
# column K is doubled. The cleaner divides only the 2011 SP values by two.
# This correction reconciles the party totals with the unit totals in column G
# and with the candidate results.

library(tidyverse)
library(here)

here::i_am("data_cleaning_cantons/canton_ZH.R") # Set a stable project root.

# Load the shared allocation and output helpers when this script runs alone.
if (!exists("allocate_hb", mode = "function") ||
    !exists("allocate_hb_with_notes", mode = "function") ||
    !exists("finalize_election_data", mode = "function")) {
  source(here("data_cleaning_cantons", "00_functions.R"))
}

if (!exists('combine_hb_seat_options', mode = 'function')) {
  source(here('data_cleaning_cantons', '00_functions.R'))
}

# Loading the raw  election files for 2007, 2011, 2015, 2019, and 2023.
# (Amt für Statistik und Daten Kanton Zürich, 2024)
zh_source_dir <- here("data", "datasets", "zurich") # Raw Zurich files.
zh_source_files <- tribble( # Match each year to its two source workbooks.
  ~year, ~party_file, ~candidate_file,
  2007L, "ZH_07_parteiresultate.xlsx", "ZH_07_kandidierendenresultate.xlsx",
  2011L, "ZH_11_parteiresultate.xlsx", "ZH_11_kandidierendenresultate.xlsx",
  2015L, "ZH_15_parteiresultate.xlsx", "ZH_15_kandidierendenresultate.xlsx",
  2019L, "ZH_19_parteiresultate.xlsx", "ZH_19_kandidierendenresultate.xlsx",
  2023L, "ZH_23_parteiresultate.xlsx", "ZH_23_kandidierendenresultate.xlsx"
)

as_zh_number <- function(value) {
  suppressWarnings(as.numeric(as.character(value))) # Convert imported text.
}

normalize_zh_label <- function(label) {
  label |>
    as.character() |> # Treat every label as text.
    str_replace_all("[\r\n]+", " ") |> # Remove line breaks.
    str_squish() |> # Remove repeated spaces.
    str_to_upper() |> # Ignore capitalization differences.
    stringi::stri_trans_general("Latin-ASCII") |> # Remove accents and umlauts.
    str_replace_all("[^A-Z0-9]+", " ") |> # Standardize punctuation.
    str_squish() # Return one clean label.
}

standardize_zh_party <- function(party_label) {
  normalized <- normalize_zh_label(party_label)

  case_when(
    normalized %in% c("GP", "GRUNE") ~ "GRUENE", # Match both Green labels.
    normalized %in% c("MITTE", "DIE MITTE") ~ "DIEMITTE", # Match Mitte labels.
    normalized %in% c("FGA AL PDA", "AL", "PDA") ~ "FGA_AL_PDA", # Join the alliance.
    normalized %in% c(
      "UBRIGE", "HANF UELI", "PIRATEN", "ERP", "IP ZH", "JUSO",
      "DIE GUTEN", "HELVIDA", "AUFL", "SAPAPO", "STOPP"
    ) ~ "UEBRIGE", # Combine minor lists reported as "Uebrige".
    TRUE ~ str_replace_all(normalized, " ", "_") # Keep other party codes.
  )
}

read_zh_election <- function(
  source_dir,
  party_file,
  candidate_file,
  election_year
) {
  party_path <- file.path(source_dir, party_file) # Complete party-file path.
  candidate_path <- file.path(source_dir, candidate_file) # Candidate-file path.
  stopifnot(file.exists(party_path), file.exists(candidate_path)) # Require both files.

  party_raw <- readxl::read_excel(party_path, col_types = "text") # Read party votes.
  candidate_raw <- readxl::read_excel(candidate_path, col_types = "text") # Read candidates.

  if (election_year == 2011L) {
    message("ZH 2011: correcting doubled SP vote totals in column K.") # Statistical Bureau of Kanton Zürich got informed of the error


  }

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
    group_by(district, unit_id) |> # Check each municipality separately.
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


  district_results <- party_observations |>
    group_by(district, parties) |> # Combine municipalities by district and party.
    summarise(party_votes = sum(party_votes), .groups = "drop") # District totals.

  elected_code <- as_zh_number(candidate_raw$Kand_Gewaehlt) # Column L: 1 = elected.
  candidate_results <- candidate_raw |>
    transmute(
      district = str_squish(Geschaeft), # Column A: voting district.
      candidate_id = str_squish(Liste_KandID), # Unique within a district.
      parties = standardize_zh_party(Liste_Code), # Column C: party code.
      elected_code,
      elected = elected_code == 1 # Convert the binary code to logical.
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
  # counting them by party gives the official DP result.
  district_seats <- candidate_results |>
    group_by(district) |> # One elected candidate equals one district seat.
    summarise(district_seats = sum(elected), .groups = "drop")

  elected_results <- candidate_results |>
    filter(elected) |> # Keep elected candidates only.
    count(parties, name = "seats_dp") # Official seats by party.

  official_results <- district_results |>
    distinct(parties) |> # Keep every party, including those with zero seats.
    left_join(elected_results, by = "parties") |> # Add official DP seats.
    transmute(
      canton = "ZH",
      year = as.integer(election_year),
      party = parties,
      seats_dp = replace_na(as.integer(seats_dp), 0L)
    )
  # Implementing the HB Allocation method used in Zurich before the reform.
  # (Kanton Zürich, 1983/2005, §§ 87–88b)

  # Under former § 88 b, an exact quotient tie first goes to the list with
  # more party votes. If party votes are also equal, the law requires a drawing
  # of lots. Since a hypothetical draw cannot be reconstructed, returning every
  # still-possible winner lets the shared allocator record the ambiguity.
  resolve_zh_hb_tie <- function(
      tied,
      votes,
      seats,
      distribution_number,
      party_names
  ) {
    tied[votes[tied] == max(votes[tied])]
  }

  hb_by_district <- district_results |>
    left_join(district_seats, by = "district") |> # Add each district magnitude.
    group_by(district) |> # Allocate seats independently in each district.
    group_modify(function(data, key) {
      allocation <- allocate_hb_with_notes(
        data$party_votes,
        first(data$district_seats),
        data$parties,
        tie_resolver = resolve_zh_hb_tie
      )

      bind_cols(data, allocation) |>
        mutate(
          hb_tie_note = if_else(
            is.na(hb_tie_note),
            NA_character_,
            paste0("District ", key$district, ": ", hb_tie_note)
          ),
          voter_number = floor(
            party_votes / first(district_seats) + 0.5
          ) # Comparable voters.
        )
    }) |>
    ungroup()

  hb_results <- hb_by_district |>
    group_by(parties) |> # Combine district results at canton level.
    summarise(
      voter_number = sum(voter_number),
      seats_hb_options = combine_hb_seat_options(seats_hb_options),
      seats_hb = unique_hb_seat_value(seats_hb_options),
      hb_tie_note = {
        notes <- unique(na.omit(hb_tie_note))
        if (length(notes) == 0L) NA_character_ else paste(notes, collapse = " | ")
      },
      .groups = "drop"
    ) |>
    mutate(vote_share = 100 * voter_number / sum(voter_number)) |> # Percent share.
    select(parties, vote_share, seats_hb, seats_hb_options, hb_tie_note)

  final <- finalize_election_data( # Apply the common canton output format.
    hb_results,
    official_results,
    "ZH",
    election_year
  )

  district_checks <- hb_by_district |>
    group_by(district) |>
    summarise(
      district_seats = first(district_seats),
      seats_hb = if (anyNA(seats_hb)) NA_integer_ else sum(seats_hb),
      has_hb_tie = any(!is.na(hb_tie_note)),
      .groups = "drop"
    )

  stopifnot(
    nrow(district_checks) == 18L,
    !anyNA(district_checks$district_seats),
    all(district_checks$district_seats > 0L),
    all(
      district_checks$has_hb_tie |
        district_checks$seats_hb == district_checks$district_seats
    ),
    sum(district_checks$district_seats) == 180L,
    nrow(final) == n_distinct(district_results$parties),
    any(grepl('/', final$seats_hb, fixed = TRUE)) ||
      sum(as.integer(final$seats_hb)) == 180L,
    sum(final$seats_dp) == 180L
  )

  final
}

process_canton_ZH <- function() {
  purrr::pmap_dfr(
    zh_source_files, # Process all registered elections and stack the results.
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
