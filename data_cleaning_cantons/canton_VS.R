# Valais-specific data cleaning and processing for the 2017, 2021 and 2025 elections.
# Processes the elections and returns the standardized cleaned table.
# Uses the shared finalizer in 00_hb_allocator.R (finalize_election_data).

# Valais is a special case because upper-tier apportionment is conducted separately
# within six electoral regions rather than canton-wide (Lüscher, 2024, p. 57).
# Under the rules applicable to each election, seats in this allocation are assigned
# to regional list groups (the temporary 2016 decree for 2017; Kanton Wallis, 2004b,
# art. 138a, version at 1 July 2018, for 2021 and 2025).
# List labels are standardized before allocation, while sub-districts and electoral
# districts remain separate. Allocated seats are then aggregated into canton-year
# analytical party-family totals.

library(tidyverse)
library(here)

here::i_am("data_cleaning_cantons/canton_VS.R")

# Load the common output helper when this file runs on its own
if (!exists("finalize_election_data", mode = "function")) {
  source(here("data_cleaning_cantons", "00_hb_allocator.R"))
}

# Sub-district magnitudes for 2017: (Kanton Wallis, 2016, p. 2)
# Sub-district magnitudes for 2021: (Kanton Wallis, 2020, p. 2)
# Sub-district magnitudes for 2025: (Kanton Wallis, 2024, p. 1)
vs_district_config <- function(election_year) {
  election_year <- as.integer(election_year)

  # Sub-district seat totals for each election year.
  district_seats <- switch(
    as.character(election_year),
    "2017" = c(
      goms = 2L, oestlich_raron = 1L, brig = 11L,
      visp = 11L, westlich_raron = 4L, leuk = 5L,
      sierre = 17L, herens = 5L, sion = 18L, conthey = 11L,
      martigny = 17L, entremont = 6L, st_maurice = 5L, monthey = 17L
    ),
    "2021" = c(
      goms = 2L, oestlich_raron = 1L, brig = 11L,
      visp = 11L, westlich_raron = 3L, leuk = 5L,
      sierre = 17L, herens = 5L, sion = 18L, conthey = 11L,
      martigny = 18L, entremont = 6L, st_maurice = 5L, monthey = 17L
    ),
    "2025" = c(
      goms = 2L, oestlich_raron = 1L, brig = 11L,
      visp = 10L, westlich_raron = 3L, leuk = 5L,
      sierre = 17L, herens = 5L, sion = 18L, conthey = 11L,
      martigny = 18L, entremont = 6L, st_maurice = 6L, monthey = 17L
    ),
    stop("Unsupported Valais election year: ", election_year)
  )

  # In this script, `district` denotes one of 14 sub-districts (Unterwahlkreise);
  # `region` denotes one of six electoral districts (Wahlkreise) for DP upper allocation.
  district_region <- c( 
    goms = "brig", oestlich_raron = "brig", brig = "brig",
    visp = "visp", westlich_raron = "visp", leuk = "visp",
    sierre = "sierre",
    herens = "sion", sion = "sion", conthey = "sion",
    martigny = "martigny", entremont = "martigny",
    st_maurice = "monthey", monthey = "monthey"
  )

  district_names <- names(district_seats)

  tibble(
    district = district_names,
    region = unname(district_region[district_names]),
    district_seats = unname(district_seats),
    file_path = here(
      "data", "datasets", "valais", paste0("VS_", election_year %% 100), # 
      paste0("VS_", election_year %% 100, "_", district_names, ".xlsx")
    )
  )
}

as_vs_number <- function(value) {
  if (is.numeric(value)) return(as.numeric(value))

  value <- str_trim(as.character(value))
  # Accept plain integers or consistent three-digit grouping with apostrophes,
  # commas or spaces. Unexpected text and decimal strings remain missing.
  valid <- !is.na(value) & (
    str_detect(value, "^[0-9]+$") |
      str_detect(
        value,
        "^[0-9]{1,3}(['\u2019, \u00A0\u202F])[0-9]{3}(?:\\1[0-9]{3})*$"
      )
  )
  parsed <- rep(NA_real_, length(value))
  parsed[valid] <- as.numeric(
    str_remove_all(value[valid], "['\u2019, \u00A0\u202F]")
  )
  parsed
}

standardize_vs_party <- function(party_label, election_year) {
  normalized <- party_label |>
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |>
    str_to_upper() |>
    str_replace_all("[^A-Z0-9]+", " ") |> 
    str_squish()

  # Use one key for the same party across Upper and Lower
  # Valais. Joint electoral lists remain separate since their list votes
  # cannot be divided reliably among the participating parties.
  # First match wins: identify joint lists and Green Liberals before broader patterns.
  party <- case_when(
    str_detect(normalized, "RASSEMBLEMENT CITOYEN") ~ "RCV",
    str_detect(normalized, "CSPO GLP") ~ "CSP_GLP",
    str_detect(normalized, "CSPO|CHRISTLICHSOZIALE") ~ "CSP",
    str_detect(normalized, "NEO") ~ "NEO",
    str_detect(normalized, "^SP UND UNAB") ~ "SP_UNABHAENGIGE",
    str_detect(normalized, "SP.*GRUENE|SP.*GRUNE|SP GRU") ~
      "SP_GRUENE_UNABHAENGIGE",
    str_detect(normalized, "GRUNLIBERAL|GRUENLIBERAL|VERT LIB") ~ "GLP",
    str_detect(normalized, "SVP|UDC") ~ "SVP",
    str_detect(normalized, "DIE MITTE OBERWALLIS|LE CENTRE") ~ "DIEMITTE",
    str_detect(
      normalized,
      "CVP.*OBERWALLIS|CHRISTLICHDEMOKRATISCHE.*OBERWALLIS"
    ) ~ "CVP",
    str_detect(normalized, "PARTI DEMOCRATE|PDC") ~ "CVP",
    str_detect(normalized, "PLR|PARTI LIBERAL|FDP") ~ "FDP",
    str_detect(normalized, "LES VERT") ~ "GRUENE",
    str_detect(normalized, "PARTI SOCIALISTE|ALLIANCE DE GAUCHE") ~
      "SP_GAUCHE_CITOYENNE",
    TRUE ~ NA_character_
  )

  if (anyNA(party)) {
    stop(
      "Unmapped Valais party label(s): ",
      paste(unique(party_label[is.na(party)]), collapse = "; ")
    )
  }

  party
}

read_vs_district <- function(
  file_path,
  district,
  region,
  district_seats,
  election_year
) {
  raw <- readxl::read_excel(
    file_path,
    sheet = "Export",
    col_names = FALSE,
    na = c("", "..."),
    .name_repair = ~ paste0("x", seq_along(.x))
  )

  # Every workbook contains municipal results and a sub-district-total column 
  party_labels <- as.character(raw[[1]])
  district_total <- raw[[ncol(raw)]]
  party_rows <- which(
    str_detect(party_labels, "^\\d+[.]?\\s+") &
      !str_detect(party_labels, "^\\d+[.]\\d+")
  )
  # The overview precedes repeated list headings in the candidate-detail section
  list_numbers <- str_extract(party_labels[party_rows], "^\\d+")
  party_rows <- party_rows[!duplicated(list_numbers)]
  party_votes <- as_vs_number(district_total[party_rows])
  
  if (
    length(party_rows) == 0L ||
    any(!is.finite(party_votes)) ||
    any(party_votes < 0) ||
    any(party_votes != floor(party_votes))
  ) {
    stop("Missing or invalid Valais list totals in: ", file_path)
  }

  district_results <- tibble(
    party_label = str_remove(party_labels[party_rows], "^\\d+[.]?\\s+"),
    party_votes = party_votes
  )

  district_results |>
    mutate(
      election_year = as.integer(election_year),
      district,
      region,
      district_seats = as.integer(district_seats),
      parties = standardize_vs_party(party_label, election_year)
    ) |>
    select(
      election_year, district, region, district_seats, parties,
      party_label, party_votes
    )
}

# The HB counterfactual follows the allocation method used in the 14 sub-districts
# before the Double-Pukelsheim reform. Lists below 8% in a sub-district are
# excluded, but their votes remain in the Hagenbach-Bischoff distribution
# number (Kanton Wallis, 2004a, arts. 154-156).
# Resolve an exact highest-average tie according to former kGPR art. 156:
# initial-allocation remainder, list votes, strongest candidate, then drawing lots.
# `draw_lots` must be a function that returns one winner from the supplied indices.

resolve_vs_hb_tie <- function(
    tied,
    votes,
    distribution_number,
    max_candidate_votes = NULL,
    draw_lots = NULL
) {
  tied <- tied[
    votes[tied] %% distribution_number ==
      max(votes[tied] %% distribution_number)
  ]
  if (length(tied) == 1L) return(tied)

  tied <- tied[votes[tied] == max(votes[tied])]
  if (length(tied) == 1L) return(tied)

  if (is.null(max_candidate_votes)) {
    stop(
      "The HB tie remains unresolved after remainders and list votes; ",
      "provide `max_candidate_votes` to apply former kGPR art. 156(1)(e)."
    )
  }
  if ( 
    !is.numeric(max_candidate_votes) ||
      length(max_candidate_votes) != length(votes) ||
      any(!is.finite(max_candidate_votes)) ||
      any(max_candidate_votes < 0) ||
      any(max_candidate_votes != floor(max_candidate_votes))
  ) {
    stop("`max_candidate_votes` must contain one non-negative integer per list.")
  }
  tied <- tied[
    max_candidate_votes[tied] == max(max_candidate_votes[tied])
  ]
  if (length(tied) == 1L) return(tied)

  if (!is.function(draw_lots)) {
    stop(
      "The HB tie also remains unresolved after candidate votes; ",
      "provide `draw_lots` to apply former kGPR art. 156(1)(f)."
    )
  }
  winner <- draw_lots(tied)
  if (length(winner) != 1L || is.na(winner) || !winner %in% tied) {
    stop("`draw_lots` must return exactly one of the supplied list indices.")
  }

  as.integer(winner)
}

# Calculate the HB counterfactual for one sub-district
allocate_vs_hb <- function(
    votes,
    n_seats,
    quorum = 0.08, # 8% threshold in each sub-district for the HB counterfactual
    max_candidate_votes = NULL,
    draw_lots = NULL
) {
  if (
    !is.numeric(votes) || any(!is.finite(votes)) || any(votes < 0) ||
      any(votes != floor(votes))
  ) {
    stop("`votes` must contain non-negative finite integers.")
  }
  if (
    length(n_seats) != 1L || !is.numeric(n_seats) || !is.finite(n_seats) ||
      n_seats < 0 || n_seats != floor(n_seats)
  ) {
    stop("`n_seats` must be one non-negative integer.")
  }
  if (
    length(quorum) != 1L || !is.numeric(quorum) || !is.finite(quorum) ||
      quorum < 0 || quorum > 1
  ) {
    stop("`quorum` must be one finite value between zero and one.")
  }

  if (n_seats == 0L) {
    return(integer(length(votes)))
  }
  if (length(votes) == 0L || sum(votes) == 0) {
    stop("At least one list must have a positive vote total.")
  }

  eligible <- votes / sum(votes) >= quorum
  if (!any(eligible)) {
    stop("No list reached the Valais district quorum.")
  }

  # Include votes from lists below the quorum in the distribution number
  distribution_number <- floor(sum(votes) / (n_seats + 1L)) + 1
  seats <- if_else(
    eligible,
    floor(votes / distribution_number),
    0
  )

  while (sum(seats) < n_seats) {
    quotients <- if_else(eligible, votes / (seats + 1L), -Inf)
    provisional_winner <- which.max(quotients)
    # Detect equal highest quotients by cross-multiplication rather than division
    tied <- which(
      eligible &
        votes * (seats[provisional_winner] + 1L) ==
          votes[provisional_winner] * (seats + 1L)
    )
    winner <- if (length(tied) == 1L) {
      tied
    } else {
      resolve_vs_hb_tie(
        tied,
        votes,
        distribution_number,
        max_candidate_votes,
        draw_lots
      )
    }
    seats[winner] <- seats[winner] + 1L
  }

  as.integer(seats)
}

# The Double-Pukelsheim upper allocation uses standard rounding (Kanton Wallis,
# 2004b, art. 155; version at 1 July 2018). Allocating seats by the highest
# weight/(seats + 0.5) quotient is the equivalent sequential form of that
# divisor method and avoids choosing a divisor by hand.
allocate_vs_dp_upper <- function(weighted_voters, n_seats) {
  if (
    !is.numeric(weighted_voters) || any(!is.finite(weighted_voters)) ||
      any(weighted_voters < 0)
  ) {
    stop("`weighted_voters` must contain non-negative finite numbers.")
  }
  if (
    length(n_seats) != 1L || !is.numeric(n_seats) || !is.finite(n_seats) ||
      n_seats < 0 || n_seats != floor(n_seats)
  ) {
    stop("`n_seats` must be one non-negative integer.")
  }

  seats <- integer(length(weighted_voters))
  if (n_seats == 0L) return(seats)
  if (length(weighted_voters) == 0L || !any(weighted_voters > 0)) {
    stop("At least one list group must have a positive weighted voter total.")
  }

  for (seat in seq_len(n_seats)) {
    quotients <- weighted_voters / (seats + 0.5)
    winner <- which.max(quotients)
    # Detect exact ties by cross-multiplication; input order must not decide a seat.
    tied <- which(
      weighted_voters * (2 * seats[winner] + 1) ==
        weighted_voters[winner] * (2 * seats + 1)
    )
    if (length(tied) > 1L) {
      stop(
        "Exact DP upper-allocation tie for seat ", seat,
        " between list-group indices ", paste(tied, collapse = ", "),
        "; explicit tie resolution is required."
      )
    }
    seats[winner] <- seats[winner] + 1L
  }

  seats
}

# Published deputy-seat totals used to validate the reconstructed upper allocation
# (Commission de validation du Grand Conseil, 2025, p. 5; Validierungskommission des Grossen Rates, 2021, p. 5).
vs_official_dp_totals <- function(election_year) {
  switch(
    as.character(election_year),
    "2021" = tribble(
      ~parties,                  ~official_voter_number, ~official_seats_dp,
      "SP_GRUENE_UNABHAENGIGE",  4056L,                   4L,
      "SVP",                    20782L,                  22L,
      "CSP",                     5651L,                   5L,
      "CSP_GLP",                 3354L,                   3L,
      "CVP",                    38360L,                  40L,
      "FDP",                    24148L,                  27L,
      "GRUENE",                 11149L,                  12L,
      "SP_GAUCHE_CITOYENNE",   13891L,                  17L
    ),
    "2025" = tribble(
      ~parties,                  ~official_voter_number, ~official_seats_dp,
      "SP_GRUENE_UNABHAENGIGE",  2856L,                   3L,
      "DIEMITTE",               32865L,                  42L,
      "SVP",                    18909L,                  26L,
      "NEO",                    6278L,                   7L,
      "FDP",                    17434L,                  27L,
      "GRUENE",                  5862L,                   8L,
      "SP_GAUCHE_CITOYENNE",   11637L,                  17L
    ),
    NULL
  )
}

# Official 2017 list seats, aggregated from the 14 sub-districts
# to the six electoral districts (Staat Wallis, 2017).

vs_official_2017_region_seats <- function() {
  tribble(
    ~region,     ~parties,                    ~official_seats_dp,
    "brig",     "CSP",                       3L,
    "brig",     "SP_GRUENE_UNABHAENGIGE",    2L,
    "brig",     "CVP",                       5L,
    "brig",     "SVP",                       4L,
    "visp",     "CSP",                       7L,
    "visp",     "CVP",                       8L,
    "visp",     "SVP",                       3L,
    "visp",     "SP_UNABHAENGIGE",            2L,
    "visp",     "FDP",                       0L,
    "sierre",   "GRUENE",                    2L,
    "sierre",   "SVP",                       3L,
    "sierre",   "FDP",                       4L,
    "sierre",   "CVP",                       5L,
    "sierre",   "SP_GAUCHE_CITOYENNE",       3L,
    "sion",     "GRUENE",                    4L,
    "sion",     "SVP",                       6L,
    "sion",     "FDP",                       8L,
    "sion",     "CVP",                      12L,
    "sion",     "SP_GAUCHE_CITOYENNE",       4L,
    "sion",     "RCV",                       0L,
    "martigny", "CVP",                       8L,
    "martigny", "SVP",                       3L,
    "martigny", "SP_GAUCHE_CITOYENNE",       4L,
    "martigny", "FDP",                       8L,
    "martigny", "RCV",                       0L,
    "martigny", "GRUENE",                    0L,
    "monthey",  "FDP",                       6L,
    "monthey",  "GRUENE",                    2L,
    "monthey",  "SP_GAUCHE_CITOYENNE",       3L,
    "monthey",  "SVP",                       4L,
    "monthey",  "CVP",                       7L,
    "monthey",  "RCV",                       0L
  )
}

process_vs_election <- function(election_year) {
  election_year <- as.integer(election_year)
  config <- vs_district_config(election_year)

  missing_files <- config$file_path[!file.exists(config$file_path)]
  if (length(missing_files) > 0L) {
    stop(
      "Missing Valais source workbook(s): ",
      paste(missing_files, collapse = "; ")
    )
  }

  stopifnot(
    nrow(config) == 14L,
    !anyNA(config$region),
    sum(config$district_seats) == 130L
  )

  district_results <- pmap_dfr(
    config,
    function(district, region, district_seats, file_path) {
      read_vs_district(
        file_path,
        district,
        region,
        district_seats,
        election_year
      )
    }
  )

  # Ensure standardized party keys identify one list per sub-district
  if (anyDuplicated(district_results[c("district", "parties")])) {
    stop("Multiple Valais lists map to the same party within a sub-district.")
  }

  # Recreate the pre-reform allocation separately in all 14 sub-districts
  hb_by_district <- district_results |>
    group_by(district) |>
    mutate(
      seats_hb = allocate_vs_hb(party_votes, first(district_seats)),
      # Round each sub-district list's voter equivalents half upward before aggregation
      voter_number = floor(party_votes / first(district_seats) + 0.5)
    ) |>
    ungroup()

  hb_results <- hb_by_district |>
    group_by(parties) |>
    summarise(
      voter_number = sum(voter_number),
      seats_hb = sum(seats_hb),
      .groups = "drop"
    ) |>
    mutate(vote_share = 100 * voter_number / sum(voter_number)) |>
    select(parties, vote_share, seats_hb)

  # A list group enters an electoral district's DP upper allocation when it reaches
  # 8% in at least one sub-district within it (Kanton Wallis, 2004b, art. 154;
  # version at 1 July 2018). Unqualified groups retain zero DP seats.
  dp_eligibility <- district_results |>
    group_by(district) |>
    mutate(district_party_share = party_votes / sum(party_votes)) |>
    ungroup() |>
    group_by(region, parties) |>
    summarise(
      qualified = any(district_party_share >= 0.08),
      # Once qualified, include rounded voter equivalents from all its sub-districts
      # within this electoral district, including those where the list is below 8%.
      weighted_voters = sum(
        floor(party_votes / district_seats + 0.5)
      ),
      .groups = "drop"
    )

  region_seats <- config |>
    group_by(region) |>
    summarise(region_seats = sum(district_seats), .groups = "drop")

  # Upper allocation determines the party totals needed for canton-wide aggregation.
  dp_by_region <- dp_eligibility |>
    left_join(region_seats, by = "region") |>
    group_by(region) |>
    mutate(
      seats_dp = {
        allocation <- integer(n())
        allocation[qualified] <- allocate_vs_dp_upper(
          weighted_voters[qualified],
          first(region_seats)
        )
        allocation
      }
    ) |>
    ungroup()

  official_results <- dp_by_region |>
    group_by(parties) |>
    summarise(seats_dp = sum(seats_dp), .groups = "drop") |>
    transmute(
      canton = "VS",
      year = election_year,
      party = parties,
      seats_dp = as.integer(seats_dp)
    )

  official_totals <- vs_official_dp_totals(election_year)
  if (!is.null(official_totals)) {
    reconstructed_voter_totals <- dp_eligibility |>
      filter(qualified) |>
      group_by(parties) |>
      summarise(
        reconstructed_voter_number = sum(weighted_voters),
        .groups = "drop"
      )

    official_check <- official_totals |>
      left_join(
        official_results |> select(parties = party, seats_dp),
        by = "parties"
      ) |>
      left_join(
        reconstructed_voter_totals,
        by = "parties"
      )

    stopifnot(
      sum(official_totals$official_seats_dp) == 130L,
      !anyNA(official_check$seats_dp),
      !anyNA(official_check$reconstructed_voter_number),
      all(
        official_check$official_voter_number ==
          official_check$reconstructed_voter_number
      ),
      all(official_check$official_seats_dp == official_check$seats_dp)
    )
  }

  # Validate every reconstructed 2017 region-party allocation against the
  # official sub-district list seats aggregated to the six electoral districts.
  if (election_year == 2017L) {
    official_2017 <- vs_official_2017_region_seats()
    official_2017_check <- official_2017 |>
      full_join(
        dp_by_region |> select(region, parties, seats_dp),
        by = c("region", "parties")
      )

    stopifnot(
      sum(official_2017$official_seats_dp) == 130L,
      !anyNA(official_2017_check$official_seats_dp),
      !anyNA(official_2017_check$seats_dp),
      all(
        official_2017_check$official_seats_dp ==
          official_2017_check$seats_dp
      )
    )
  }

  final <- finalize_election_data(
    hb_results,
    official_results,
    "VS",
    election_year
  )

  district_checks <- hb_by_district |>
    group_by(district) |>
    summarise(
      district_seats = first(district_seats),
      seats_hb = sum(seats_hb),
      .groups = "drop"
    )

  stopifnot(
    n_distinct(district_results$district) == 14L,
    all(district_results$party_votes >= 0),
    all(district_checks$seats_hb == district_checks$district_seats),
    sum(as.integer(final$seats_hb)) == 130L,
    sum(final$seats_dp) == 130L
  )

  final
}

process_canton_VS <- function() {
  bind_rows(
    process_vs_election(2017),
    process_vs_election(2021),
    process_vs_election(2025)
  )
}
