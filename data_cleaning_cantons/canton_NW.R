# Nidwalden-specific data cleaning for the 2014, 2018, 2022 and 2026 elections.
# Processes the elections and returns the standardized cleaned table.
# Using shared helpers: 00_hb_allocator.R (allocate_hb, finalize_election_data).

library(tidyverse)
library(here)

# Make all file references independent of the current working directory
here::i_am("data_cleaning_cantons/canton_NW.R")

# Load the shared allocation and output-format helpers when this file is run
# on its own rather than sourced after 00_hb_allocator.R.
if (!exists("allocate_hb", mode = "function") ||
    !exists("finalize_election_data", mode = "function")) {
  source(here("data_cleaning_cantons", "00_hb_allocator.R"))
}

# Convert workbook labels to stable ASCII text before matching them. This
# handles punctuation, line breaks, and spelling changes between elections.
normalize_nw_label <- function(label) {
  label |>
    str_replace_all("[\r\n]+", " ") |>
    str_squish() |>
    str_to_upper() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_replace_all("[^A-Z0-9]+", " ") |>
    str_squish()
}

standardize_nw_party <- function(party_label) {
  normalized <- normalize_nw_label(party_label)

  case_when(
    normalized %in% c("GN", "GRUNE", "GRUNE NIDWALDEN") ~ "GRUENE",
    normalized %in% c("GLP", "GRUNLIBERALE PARTEI","GRUNLIBERALE PARTEI NIDWALDEN") ~ "GLP",
    normalized %in% c("FDP", "FDP DIE LIBERALEN") ~ "FDP",
    normalized %in% c("DIE MITTE", "DIE MITTE NIDWALDEN") ~ "DIEMITTE",
    normalized %in% c("SP", "SP NIDWALDEN", "SP JUSO") ~ "SP",
    normalized == "SVP NIDWALDEN" ~ "SVP",
    normalized == "NEMITZ" | str_starts(normalized, "PIERRE NEMITZ") ~ "NEMITZ",
    TRUE ~ str_replace_all(normalized, " ", "_")
  )
}

read_nw_sheet <- function(file_name, sheet_name) {
  readxl::read_excel(
    here("data", "datasets", "nidwalden", file_name),
    sheet = sheet_name,
    col_names = FALSE,
    na = c("", "..."),
    .name_repair = ~ paste0("x", seq_along(.x))
  )
}

as_nw_number <- function(x) {
  suppressWarnings(as.numeric(x))
}

find_nw_row <- function(data, label) {
  matching_rows <- which(normalize_nw_label(data[[1]]) == label)
  stopifnot(length(matching_rows) == 1L)
  matching_rows
}

read_nw_election <- function(file_name, election_year) {
  basis_raw <- read_nw_sheet(file_name, "Basiszahlen")
  upper_raw <- read_nw_sheet(file_name, "Oberzuteilung")
  lower_raw <- read_nw_sheet(file_name, "Unterzuteilung")

  
  # Basiszahlen: district magnitudes, party votes, and party voter numbers
  basis_header_row <- find_nw_row(basis_raw, "GEMEINDE")
  basis_total_row <- find_nw_row(basis_raw, "TOTAL") # Cantonal totals for each party
  basis_subheader <- normalize_nw_label(as.character(unlist(
    basis_raw[basis_header_row + 1L, ],
    use.names = FALSE
  )))
  basis_party_cols <- which(basis_subheader == "STIMMEN")  # Each party's voter-equivalent column immediately follows its vote column
  basis_header <- as.character(unlist(
    basis_raw[basis_header_row, ],
    use.names = FALSE
  ))
  basis_parties <- standardize_nw_party(basis_header[basis_party_cols])
  
  # Structural checks on the extracted party columns
  stopifnot(
    length(basis_party_cols) > 0L,
    all(basis_party_cols + 1L <= ncol(basis_raw)),
    !anyNA(basis_parties),
    !anyDuplicated(basis_parties)
  )

  possible_district_rows <- seq.int(
    basis_header_row + 1L,
    basis_total_row - 1L
  )
  possible_district_seats <- as_nw_number(
    basis_raw[[2]][possible_district_rows]
  ) # District rows are identified by a numeric seat count
  district_rows <- possible_district_rows[!is.na(possible_district_seats)]

  district_results <- tibble(
    district = str_squish(as.character(basis_raw[[1]][district_rows])),
    district_seats = as.integer(as_nw_number(basis_raw[[2]][district_rows]))
  )

  for (party_index in seq_along(basis_party_cols)) {
    party <- basis_parties[[party_index]]
    raw_votes <- basis_raw[[basis_party_cols[[party_index]]]][district_rows]
    numeric_votes <- as_nw_number(raw_votes)

    if (any(!is.na(raw_votes) & is.na(numeric_votes))) {
      stop(
        "Non-numeric NW party votes in election ",
        election_year, " for ", party, ".",
        call. = FALSE
      )
    }

    district_results[[party]] <- replace_na(numeric_votes, 0)
  }

  basis_totals <- tibble(
    party = basis_parties,
    party_votes_basis = as_nw_number(as.character(unlist(
      basis_raw[basis_total_row, basis_party_cols],
      use.names = FALSE
    ))),
    voter_number_basis = as_nw_number(as.character(unlist(
      basis_raw[basis_total_row, basis_party_cols + 1L],
      use.names = FALSE
    )))
  )

  # Check that district votes add up to the reported cantonal totals
  stopifnot(
    all(
      colSums(district_results[basis_parties]) ==
        basis_totals$party_votes_basis
    )
  )
 
  # Oberzuteilung: reported official numbers and official cantonal seat totals
  upper_header_row <- find_nw_row(upper_raw, "LISTE")
  upper_header <- normalize_nw_label(as.character(unlist(
    upper_raw[upper_header_row, ],
    use.names = FALSE
  )))
  upper_vote_col <- which(upper_header == "STIMMEN")
  upper_voter_col <- which(upper_header == "WAHLERZAHL") # Voter equivalents column
  upper_quotient_col <- which(upper_header == "QUOTIENT")
  upper_seat_col <- which(upper_header == "SITZANSPRUCH") # Upper apportionment seat entitlement

  #Structural checks on the extracted columns
  stopifnot(
    length(upper_vote_col) == 1L,
    length(upper_voter_col) == 1L,
    length(upper_quotient_col) == 1L,
    length(upper_seat_col) == 1L
  )

  possible_upper_rows <- seq.int(upper_header_row + 1L, nrow(upper_raw))
  possible_voter_numbers <- as_nw_number(
    upper_raw[[upper_voter_col]][possible_upper_rows]
  )
  upper_rows <- possible_upper_rows[!is.na(possible_voter_numbers)]

  # Create a tibble of the upper apportionment results
  upper_results <- tibble(
    party = standardize_nw_party(upper_raw[[1]][upper_rows]),
    party_votes_upper = as_nw_number(upper_raw[[upper_vote_col]][upper_rows]),
    voter_number = as_nw_number(upper_raw[[upper_voter_col]][upper_rows]),
    # The reported seat quotient column, is retained for checks
    reported_quotient = as_nw_number(
      upper_raw[[upper_quotient_col]][upper_rows]
    ),
    seats_dp_upper = as.integer(
      as_nw_number(upper_raw[[upper_seat_col]][upper_rows])
    )
  )

  basis_upper_check <- basis_totals |>
    full_join(upper_results, by = "party") # Retain unmatched parties so the checks below detect differences between sheets

  # Structural checks on the joined data
  stopifnot(
    nrow(basis_upper_check) == length(basis_parties),
    !anyNA(basis_upper_check),
    all(basis_upper_check$party_votes_basis ==
          basis_upper_check$party_votes_upper),
    all(basis_upper_check$voter_number_basis ==
          basis_upper_check$voter_number),
    all(upper_results$reported_quotient >= 0)
  )

  # Computing each party's vote share based on the "Wählerzahl" (voter equivalents)
  vote_shares <- upper_results |>
    transmute(
      parties = party,
      vote_share = 100 * voter_number / sum(voter_number)
    )

  # Unterzuteilung: official Double-Pukelsheim seats
  lower_header_row <- find_nw_row(lower_raw, "GEMEINDE")
  lower_total_row <- find_nw_row(lower_raw, "TOTAL")
  lower_subheader <- normalize_nw_label(as.character(unlist(
    lower_raw[lower_header_row + 1L, ],
    use.names = FALSE
  )))
  lower_party_cols <- which(lower_subheader == "STIMMEN")
  lower_header <- as.character(unlist(
    lower_raw[lower_header_row, ],
    use.names = FALSE
  ))
  lower_parties <- standardize_nw_party(lower_header[lower_party_cols])

  # Create tibble with the DP seats for each party
  observed_dp <- tibble(
    party = lower_parties,
    seats_dp = as.integer(as_nw_number(as.character(unlist(
      lower_raw[lower_total_row, lower_party_cols + 1L],
      use.names = FALSE
    ))))
  )

  # Keep parties with zero DP seats, because they could win seats under HB.
  # Check their DP seats against "Oberzuteilung" upper apportionment below.
  official_results <- tibble(party = basis_parties) |>
    left_join(observed_dp, by = "party") |>
    mutate(
      canton = "NW",
      year = as.integer(election_year),
      seats_dp = replace_na(seats_dp, 0L) # Replace missing DP seat values with zero
    ) |>
    select(canton, year, party, seats_dp)

  dp_check <- official_results |>
    left_join(
      upper_results |> select(party, seats_dp_upper),
      by = "party"
    )

  stopifnot(
    !anyNA(dp_check$seats_dp_upper),
    all(dp_check$seats_dp == dp_check$seats_dp_upper)
  )

  # Counterfactual Hagenbach-Bischoff allocation according to 
  # the previously used method in Nidwalden (BGE 136 I 352 E. 3.1).
  hb_summary <- district_results |>
    pivot_longer(
      cols = all_of(basis_parties),
      names_to = "parties",
      values_to = "party_votes"
    ) |>
    group_by(district) |>
    mutate(
      seats_hb = allocate_hb(party_votes, first(district_seats)) # The same district magnitude is repeated on every party row
    ) |>
    ungroup() |>
    group_by(parties) |>
    summarise(seats_hb = sum(seats_hb), .groups = "drop")

  
  final <- finalize_election_data(
    vote_shares |> left_join(hb_summary, by = "parties"),
    official_results,
    "NW",
    election_year
  )
  # Structural checks on the final election results
  stopifnot(
    nrow(district_results) == 11L,
    !anyNA(district_results$district_seats),
    sum(district_results$district_seats) == 60L,
    nrow(final) == length(basis_parties),
    sum(as.integer(final$seats_hb)) == 60L,
    sum(final$seats_dp) == 60L
  )

  final
}

process_canton_NW <- function() {
  # Workbook containing Basiszahlen, Oberzuteilung, and Unterzuteilung (Staatskanzlei Nidwalden, 2014)
  nw_14_final <- read_nw_election("nw_2014_basiszahlen.xlsx", 2014)

  # Workbook containing Basiszahlen, Oberzuteilung, and Unterzuteilung (Staatskanzlei Nidwalden, 2018)
  nw_18_final <- read_nw_election("nw_2018_basiszahlen.xlsx", 2018)

  # Workbook containing Basiszahlen, Oberzuteilung, and Unterzuteilung (Staatskanzlei Nidwalden, 2022)
  nw_22_final <- read_nw_election("nw_2022_basiszahlen.xlsx", 2022)

  # Workbook containing Basiszahlen, Oberzuteilung, and Unterzuteilung (Staatskanzlei Nidwalden, 2026)
  nw_26_final <- read_nw_election("nw_2026_basiszahlen.xlsx", 2026)

# The data was obtained via email from Staatskanzlei Nidwalden (Kanzleisekretariat), and is not publicly available.
 
  bind_rows(
    nw_14_final,
    nw_18_final,
    nw_22_final,
    nw_26_final
  )
}
