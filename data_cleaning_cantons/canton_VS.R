# Valais-specific data cleaning and processing
# Processes the 2017, 2021, and 2025 elections and returns the standardized table

library(tidyverse)
library(readxl)
library(here)

# Make all file references independent of the current working directory.
here::i_am("data_cleaning_cantons/canton_VS.R")

# Load the common output helper when this file runs on its own.
if (!exists("finalize_election_data", mode = "function")) {
  source(here("data_cleaning_cantons", "00_functions.R"))
}

# Official district magnitudes:
# 2017: https://www.vs.ch/documents/529400/2263784/MM_Verteilung_Grossratssitze.pdf/30be6442-6e9e-4d91-adef-76d3f60d6107
# 2021: https://www.vs.ch/documents/529400/8932772/2020+09+25+-+MM+-+Verteilung+Grossratssitze.pdf/62d59d10-2075-5231-e385-88688c7cb559
# 2025: https://www.vs.ch/documents/529400/33671442/Anhang+Verteilung.pdf/b1f92f33-f4b2-35ef-6149-838af6c70b09
vs_district_config <- function(election_year) {
  election_year <- as.integer(election_year)

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
      "data", "datasets", "wallis", paste0("VS_", election_year %% 100),
      paste0("VS_", election_year %% 100, "_", district_names, ".xlsx")
    )
  )
}

as_vs_number <- function(value) {
  value <- as.character(value)
  parsed <- str_replace_all(value, "[^0-9.-]", "")
  suppressWarnings(as.numeric(na_if(parsed, "")))
}

standardize_vs_party <- function(party_label, election_year) {
  normalized <- party_label |>
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |>
    str_to_upper() |>
    str_replace_all("[^A-Z0-9]+", " ") |>
    str_squish()

  # Use one language-neutral key for the same party across Upper and Lower
  # Valais. Joint electoral lists remain separate because their list votes
  # cannot be divided reliably among the participating parties.
  party <- case_when(
    str_detect(normalized, "RASSEMBLEMENT CITOYEN") ~ "RCV",
    str_detect(normalized, "CSPO|CHRISTLICHSOZIALE") ~ if_else(
      election_year == 2021L, "CSP_GLP", "CSP"
    ),
    str_detect(normalized, "NEO") ~ "NEO",
    str_detect(normalized, "SP.*GRUENE|SP.*GRUNE|SP UND UNAB|SP GRU") ~
      "SP_GRUENE_UNABHAENGIGE",
    str_detect(normalized, "GRUNLIBERAL|GRUENLIBERAL|VERT LIB") ~ "GLP",
    str_detect(normalized, "SVP|UDC") ~ "SVP",
    str_detect(normalized, "DIE MITTE OBERWALLIS|LE CENTRE") ~ "MITTE",
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

  # Three 2021 files are official result summaries; the remaining workbooks
  # contain municipal results and a district-total column at the far right.
  is_result_summary <- str_squish(as.character(raw[[1]][1])) == "Ergebnisse"

  if (is_result_summary) {
    party_rows <- which(str_detect(
      as.character(raw[[4]]),
      regex("^Sitze\\s*:", ignore_case = TRUE)
    ))

    district_results <- tibble(
      party_label = as.character(raw[[3]][party_rows]),
      party_votes = as_vs_number(raw[[5]][party_rows]),
      reported_seats_dp = as.integer(as_vs_number(raw[[4]][party_rows]))
    )
  } else {
    party_labels <- as.character(raw[[1]])
    district_total <- raw[[ncol(raw)]]
    party_rows <- which(
      str_detect(party_labels, "^\\d+[.]?\\s+") &
        !str_detect(party_labels, "^\\d+[.]\\d+") &
        !is.na(as_vs_number(district_total))
    )

    district_results <- tibble(
      party_label = str_remove(
        party_labels[party_rows],
        "^\\d+[.]?\\s+"
      ),
      party_votes = as_vs_number(district_total[party_rows]),
      reported_seats_dp = NA_integer_
    )
  }

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
      party_label, party_votes, reported_seats_dp
    )
}

# The former Valais rule excluded lists below 8% in the district, while still
# including their votes in the Hagenbach-Bischoff distribution number
# (kGPR 160.1, version at 9 April 2017, arts. 154-156).
allocate_vs_hb <- function(votes, n_seats, quorum = 0.08) {
  stopifnot(
    is.numeric(votes), all(is.finite(votes)), all(votes >= 0),
    length(n_seats) == 1L, n_seats >= 0
  )

  if (n_seats == 0L) {
    return(integer(length(votes)))
  }

  eligible <- votes / sum(votes) >= quorum
  if (!any(eligible)) {
    stop("No list reached the Valais district quorum.")
  }

  distribution_number <- floor(sum(votes) / (n_seats + 1L)) + 1
  seats <- if_else(
    eligible,
    floor(votes / distribution_number),
    0
  )

  while (sum(seats) < n_seats) {
    quotients <- if_else(eligible, votes / (seats + 1L), -Inf)
    winner <- which.max(quotients)
    seats[winner] <- seats[winner] + 1L
  }

  as.integer(seats)
}

# The Double-Pukelsheim upper allocation uses standard rounding. Allocating
# seats by the highest weight/(seats + 0.5) quotient is the equivalent
# sequential form of that divisor method and avoids choosing a divisor by hand.
allocate_vs_dp_upper <- function(weighted_voters, n_seats) {
  stopifnot(
    is.numeric(weighted_voters), all(is.finite(weighted_voters)),
    all(weighted_voters >= 0), length(n_seats) == 1L, n_seats >= 0
  )

  seats <- integer(length(weighted_voters))
  for (seat in seq_len(n_seats)) {
    quotients <- weighted_voters / (seats + 0.5)
    winner <- which.max(quotients)
    seats[winner] <- seats[winner] + 1L
  }

  seats
}

# Published deputy-seat totals used to validate the reconstructed upper
# allocation. Sources: the Grand Council validation-commission reports for the
# constitutive sessions of April 2021 and April 2025.
vs_official_dp_totals <- function(election_year) {
  switch(
    as.character(election_year),
    "2021" = tribble(
      ~parties,                  ~official_voter_number, ~official_seats_dp,
      "SP_GRUENE_UNABHAENGIGE",  4056L,                   4L,
      "SVP",                    20782L,                  22L,
      "CSP_GLP",                 9005L,                   8L,
      "CVP",                    38360L,                  40L,
      "FDP",                    24148L,                  27L,
      "GRUENE",                 11149L,                  12L,
      "SP_GAUCHE_CITOYENNE",   13891L,                  17L
    ),
    "2025" = tribble(
      ~parties,                  ~official_voter_number, ~official_seats_dp,
      "SP_GRUENE_UNABHAENGIGE",  2856L,                   3L,
      "MITTE",                  32865L,                  42L,
      "SVP",                    18909L,                  26L,
      "NEO",                    6278L,                   7L,
      "FDP",                    17434L,                  27L,
      "GRUENE",                  5862L,                   8L,
      "SP_GAUCHE_CITOYENNE",   11637L,                  17L
    ),
    NULL
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

  # Recreate the pre-reform allocation separately in all 14 districts.
  hb_by_district <- district_results |>
    group_by(district) |>
    mutate(
      seats_hb = allocate_vs_hb(party_votes, first(district_seats)),
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

  # A list group enters a region's upper allocation when it reaches 8% in at
  # least one of that region's districts. Only eligible groups enter the
  # standard-rounding allocation; unqualified groups retain zero DP seats.
  dp_eligibility <- district_results |>
    group_by(district) |>
    mutate(district_party_share = party_votes / sum(party_votes)) |>
    ungroup() |>
    group_by(region, parties) |>
    summarise(
      qualified = any(district_party_share >= 0.08),
      weighted_voters = sum(
        floor(party_votes / district_seats + 0.5)
      ),
      .groups = "drop"
    )

  region_seats <- config |>
    group_by(region) |>
    summarise(region_seats = sum(district_seats), .groups = "drop")

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

  # The official 2017 Brig-region result is also available as a published
  # four-party matrix and validates the first election's reconstruction.
  if (election_year == 2017L) {
    brig_official <- tribble(
      ~parties,                    ~official_seats_dp,
      "CSP",                      3L,
      "SP_GRUENE_UNABHAENGIGE",   2L,
      "CVP",                      5L,
      "SVP",                      4L
    )
    brig_check <- brig_official |>
      left_join(
        dp_by_region |>
          filter(region == "brig") |>
          select(parties, seats_dp),
        by = "parties"
      )

    stopifnot(
      !anyNA(brig_check$seats_dp),
      all(brig_check$official_seats_dp == brig_check$seats_dp)
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

  reported_checks <- district_results |>
    filter(!is.na(reported_seats_dp)) |>
    group_by(district) |>
    summarise(
      district_seats = first(district_seats),
      reported_seats_dp = sum(reported_seats_dp),
      .groups = "drop"
    )

  # Sierre consists of one district, so its reported 2021 list seats also
  # provide a direct check of the reconstructed upper allocation.
  if (election_year == 2021L) {
    sierre_reported <- district_results |>
      filter(district == "sierre") |>
      select(parties, reported_seats_dp)
    sierre_reconstructed <- dp_by_region |>
      filter(region == "sierre") |>
      select(parties, seats_dp)
    sierre_check <- full_join(
      sierre_reported,
      sierre_reconstructed,
      by = "parties"
    )

    stopifnot(
      nrow(reported_checks) == 3L,
      all(reported_checks$reported_seats_dp == reported_checks$district_seats),
      all(sierre_check$reported_seats_dp == sierre_check$seats_dp)
    )
  }

  stopifnot(
    n_distinct(district_results$district) == 14L,
    all(district_results$party_votes >= 0),
    all(district_checks$seats_hb == district_checks$district_seats),
    sum(final$seats_hb) == 130L,
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

# Temporary check - delete after use.
print(process_canton_VS(), n = Inf)
