# Uri-specific data cleaning for the 2020 and 2024 Landrat elections.
# Only the 4 municipalities with proportional representation are included in the analysis.
# This because they are the ones showing the effects of the double Pukelsheim system.
# The 4 municipalities elect 37 of the Landrat's 64 members.


# Load the necessary packages
library(tidyverse)
library(readxl)
library(here)

# Make all file references independent of the current working directory.
here::i_am("data_cleaning_cantons/canton_UR.R")

# Load the shared allocation and output-format helpers when this file runs on
# its own rather than after 00_functions.R.
if (!exists("allocate_hb_with_notes", mode = "function") ||
    !exists("combine_hb_seat_options", mode = "function") ||
    !exists("unique_hb_seat_value", mode = "function") ||
    !exists("finalize_election_data", mode = "function") ||
    !"parliament_size" %in% names(formals(finalize_election_data))) {
  source(here("data_cleaning_cantons", "00_functions.R"))
}

# Load the election data for 2020 and 2024
# (Standeskanzlei Uri, 2024a)
ur_source_path <- here(
  "data", "datasets", "uri", "UR_24_auswertung.xlsx" # The 2024 file also contains the 2020 results.
)
ur_source_sheet <- "722 Verteilung Gden"

# NOTE "FL" means "Freie Listen" and therefore ballots without a party/list designation.
# It's not an own Party. (Standeskanzlei Uri, 2024b, p. 20)


ur_election_columns <- tribble( # create a tibble with the column positions for each year
  ~year, ~district_vote_col, ~district_dp_col, ~party_vote_col, ~party_share_col, ~party_dp_col,
  2020L,                  4L,               10L,              19L,                 20L,            25L,
  2024L,                  6L,               12L,              21L,                 22L,            27L
)

ur_district_blocks <- tribble( # create a tibble with the row positions for each district's block of data
  ~district,    ~header_row, ~first_party_row, ~last_party_row,
  "Altdorf",            37L,              38L,             42L,
  "Bürglen",            43L,              44L,             48L,
  "Erstfeld",           49L,              50L,             54L,
  "Schattdorf",         55L,              56L,             60L
)

as_ur_number <- function(value) { # convert a value to numeric, suppressing warnings for non-numeric values
  suppressWarnings(as.numeric(as.character(value)))
}

# Make labels comparable despite line breaks, accents, or extra spaces.
normalize_ur_label <- function(label) {
  label |>
    as.character() |>
    str_replace_all("[\r\n]+", " ") |>
    str_squish() |>
    str_to_upper() |>
    stringi::stri_trans_general("Latin-ASCII")
}

# Keep the 2020 CVP label, but use the post-renaming analytical label in 2024.
# The workbook abbreviates both years as CVP; the 2024 official list was
# "CVP - Die Mitte Uri" (Standeskanzlei Uri, 2024b, section 2.1).
standardize_ur_party <- function(party_label, election_year) {
  normalized <- normalize_ur_label(party_label)

  case_when(
    str_detect(normalized, "^CVP(?: |$)|^DIE MITTE(?: |$)") ~
      if_else(election_year >= 2024L, "DIEMITTE", "CVP"),
    str_detect(normalized, "^FDP(?: |$)") ~ "FDP",
    str_detect(normalized, "^GLP(?: |$)") ~ "GLP",
    str_detect(normalized, "^SP(?:/G|/GB|/GRUNE| |$)") ~ "SP_GRUENE_GBU",
    str_detect(normalized, "^SVP(?: |$)") ~ "SVP",
    TRUE ~ normalized |>
      str_replace_all("[^A-Z0-9]+", "_") |>
      str_remove("^_+") |>
      str_remove("_+$")
  )
}


# Compute the HB Seats according to the prior Uri Proporzgesetz.
# (Kanton Uri, 2018, Art. 22–23, “Bisher”)

# Break an HB tie by comparing remainders first and party votes second.
# If it is still tied, keep all possible winners instead of choosing one.
resolve_ur_hb_tie <- function(
    tied,
    votes,
    seats,
    distribution_number,
    party_names
) {
  remainders <- votes %% distribution_number
  admissible <- tied[
    remainders[tied] == max(remainders[tied])
  ]

  if (length(admissible) > 1L) {
    admissible <- admissible[
      votes[admissible] == max(votes[admissible])
    ]
  }

  admissible
}

# Calculate HB seats in each municipality, then add them up by party.
summarize_ur_hb <- function(district_results) {
  district_results |>
    group_by(district) |>
    group_modify(function(data, key) {
      allocation <- allocate_hb_with_notes(
        data$party_votes,
        first(data$district_seats),
        data$parties,
        tie_resolver = resolve_ur_hb_tie
      )

      bind_cols(data, allocation) |>
        mutate(
          hb_tie_note = if_else(
            is.na(hb_tie_note),
            NA_character_,
            paste0("Municipality ", key$district, ": ", hb_tie_note)
          )
        )
    }) |>
    ungroup() |>
    group_by(parties) |>
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

read_ur_election <- function(election_year) {
  source_columns <- ur_election_columns |>
    filter(year == as.integer(election_year))
  stopifnot(nrow(source_columns) == 1L, file.exists(ur_source_path))

  # Read without headers because the needed values are in fixed Excel cells.
  raw <- read_excel(
    ur_source_path,
    sheet = ur_source_sheet,
    col_names = FALSE,
    na = c("", "..."),
    .name_repair = ~ paste0("x", seq_along(.x))
  )

  stopifnot(nrow(raw) >= 62L, ncol(raw) >= 27L)

  # Select the four absolute vote and seat columns for this election year.
  selected_columns <- unlist(
    source_columns[
      c("district_vote_col", "district_dp_col", "party_vote_col", "party_dp_col")
    ],
    use.names = FALSE
  ) |>
    as.integer()
  column_years <- vapply(
    selected_columns,
    function(column) as_ur_number(raw[[column]][34L]),
    numeric(1L)
  )
  column_units <- vapply(
    selected_columns,
    function(column) normalize_ur_label(raw[[column]][35L]),
    character(1L)
  )
  # Stop if the chosen columns do not match the requested year or unit.
  stopifnot(
    all(column_years == election_year),
    all(column_units == "ABS."),
    normalize_ur_label(raw[[source_columns$party_share_col]][35L]) == "%"
  )

  party_rows <- 37L:41L
  source_parties <- standardize_ur_party(raw[[16L]][party_rows], election_year)
  stopifnot(!anyNA(source_parties), !anyDuplicated(source_parties))

  # Read the overall party totals used later to check the municipal data.
  reported_party_results <- tibble(
    parties = source_parties,
    party_votes_reported = as_ur_number(
      raw[[source_columns$party_vote_col]][party_rows]
    ),
    vote_share_reported = as_ur_number(
      raw[[source_columns$party_share_col]][party_rows]
    ),
    seats_dp_reported = as_ur_number(
      raw[[source_columns$party_dp_col]][party_rows]
    )
  )

  # Extract votes and official DP seats from all four municipalities.
  district_results <- purrr::pmap_dfr(
    ur_district_blocks,
    function(district, header_row, first_party_row, last_party_row) {
      rows <- seq.int(first_party_row, last_party_row)
      block_parties <- standardize_ur_party(raw[[1L]][rows], election_year)

      stopifnot(
        normalize_ur_label(raw[[1L]][header_row]) == normalize_ur_label(district),
        identical(block_parties, source_parties)
      )

      tibble(
        district = district,
        parties = block_parties,
        party_votes = replace_na(
          as_ur_number(raw[[source_columns$district_vote_col]][rows]),
          0
        ),
        seats_dp_district = replace_na(
          as_ur_number(raw[[source_columns$district_dp_col]][rows]),
          0
        )
      )
    }
  )


  # Remove party rows that contain no votes anywhere in this election.
  active_parties <- district_results |>
    group_by(parties) |>
    summarise(party_votes = sum(party_votes), .groups = "drop") |>
    filter(party_votes > 0) |>
    pull(parties)

  district_results <- district_results |>
    filter(parties %in% active_parties) |>
    group_by(district) |>
    mutate(
      # Adding the official DP seats gives the municipality's seat total.
      district_seats = as.integer(sum(seats_dp_district))
    ) |>
    ungroup() |>
    mutate(
      party_votes = as.numeric(party_votes),
      seats_dp_district = as.integer(seats_dp_district)
    )

  # Check that the four municipal blocks are complete and internally valid.
  district_checks <- district_results |>
    group_by(district) |>
    summarise(
      district_seats = first(district_seats),
      seat_values = n_distinct(district_seats),
      seats_dp = sum(seats_dp_district),
      .groups = "drop"
    )
  stopifnot(
    nrow(district_checks) == 4L,
    all(district_checks$seat_values == 1L),
    all(district_checks$district_seats > 0L),
    all(district_checks$seats_dp == district_checks$district_seats),
    sum(district_checks$district_seats) == 37L,
    !anyDuplicated(district_results[c("district", "parties")]),
    all(district_results$party_votes >= 0),
    all(district_results$party_votes == floor(district_results$party_votes))
  )

  # Add the municipal votes and DP seats together for each party.
  aggregated_results <- district_results |>
    group_by(parties) |>
    summarise(
      party_votes = sum(party_votes),
      seats_dp = sum(seats_dp_district),
      .groups = "drop"
    )

  # Reconcile the sum of the four municipal blocks with the separate party
  # totals and official Double-Pukelsheim seat totals.
  source_check <- aggregated_results |>
    left_join(
      reported_party_results |> filter(parties %in% active_parties),
      by = "parties"
    )
  stopifnot(
    !anyNA(source_check),
    all(source_check$party_votes == source_check$party_votes_reported),
    all(source_check$seats_dp == source_check$seats_dp_reported),
    abs(sum(source_check$vote_share_reported) - 100) < 1e-8,
    sum(source_check$seats_dp) == 37L
  )

  # The Excel vote shares in the Excel are already based only on Altdorf, Bürglen,
  # Erstfeld, and Schattdorf. Therefore they can be directly used.

  vote_shares <- reported_party_results |>
    filter(parties %in% active_parties) |>
    transmute(
      parties,
      vote_share = as.numeric(vote_share_reported)
    )

  # Prepare the official DP results in the common canton-year format.
  official_results <- aggregated_results |>
    transmute(
      canton = "UR",
      year = as.integer(election_year),
      party = parties,
      seats_dp = as.integer(seats_dp)
    )

  # Combine the calculated former-law seats with the vote shares.
  hb_results <- vote_shares |>
    left_join(summarize_ur_hb(district_results), by = "parties")

  # Convert everything to the same final structure used for all cantons.
  final <- finalize_election_data(
    hb_results,
    official_results,
    "UR",
    election_year,
    parliament_size = 64L
  )

  # Make sure both systems allocate all 37 proportional seats exactly once.
  stopifnot(
    nrow(final) == length(active_parties),
    sum(final$seats_dp) == 37L,
    all(!str_detect(final$seats_hb, "/")),
    sum(as.integer(final$seats_hb)) == 37L
  )

  final
}

# Process 2020 and 2024 and combine both results into one table.
process_canton_UR <- function() {
  purrr::map_dfr(
    ur_election_columns$year,
    read_ur_election
  )
}

# Temporary check - delete in the end.
options(width = 200)
print(process_canton_UR(), n = 200)
