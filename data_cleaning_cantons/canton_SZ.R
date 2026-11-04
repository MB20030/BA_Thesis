# Schwyz-specific data cleaning for the 2016, 2020, and 2024 elections.
# Processes the elections and returnes the standardized cleaned table.
# Using shared helpers: 00_hb_allocator.R (allocate_hb, finalize_election_data).

# The file structure varies heavily throghout the elections, thus they are processed separately.

library(tidyverse)
library(readxl)
library(here)

here::i_am("data_cleaning_cantons/canton_SZ.R") 

# Load shared helpers when they are not already available
required_helpers <- c(
  "validate_hb_inputs",
  "hb_allocation_outcomes",
  "allocate_hb",
  "allocate_hb_with_notes",
  "combine_hb_seat_options",
  "unique_hb_seat_value",
  "finalize_election_data"
)

helpers_available <- vapply(
  required_helpers,
  function(name) exists(name, mode = "function", inherits = TRUE),
  logical(1)
)
# Check if helpers match their expectation
if (!all(helpers_available) ||
    !"parliament_size" %in% names(formals(finalize_election_data))) {
  source(here("data_cleaning_cantons", "00_hb_allocator.R"), local = TRUE)
}

# loading election datasets from canton Schwyz
# (Staatskanzlei Kanton Schwyz, 2016, 2020, 2024)
sz_source_dir <- here("data", "datasets", "schwyz") 
sz_source_files <- list(
  election_2016 = paste0(                                  
    "SZ_16_",
    c( # The Schwyz municipialites/constituencies in 2016
      "alpthal", "altendorf", "arth", "einsiedeln", "feusisberg", "freienbach",
      "galgenen", "gersau", "illgau", "ingenbohl", "innerthal", "kuessnacht",
      "lachen", "lauerz", "morschach", "moutathal", "oberiberg", "reichenburg",
      "riemenstalden", "rothenturm", "sattel", "schwyz", "schuebelbach", "steinen",
      "steinerberg", "tuggen", "unteriberg", "vorderthal", "wangen", "wollerau"
    ),
    ".csv"
  ),
  election_2020 = "SZ_20_kandidierendenresultate.csv",  
  election_2024 = "SZ_24_kantonsprotokoll.xlsx"          
)

as_sz_number <- function(value) {
  value <- as.character(value)
  number <- suppressWarnings(as.numeric(value)) # Unconvertible values become NA; conversion warnings are suppressed

  unexpected <- !is.na(value) &
    nzchar(trimws(value)) &
    !is.finite(number)

  if (any(unexpected)) {
    stop(
      "Unexpected value in a numeric field: ",
      paste(unique(value[unexpected]), collapse = ", ")
    )
  }

  number
}
standardize_sz_party <- function(party_label) {
  normalized <- party_label |>
    as.character() |>
    str_replace_all("[\r\n]+", " ") |>
    str_squish() |>
    str_to_upper() |>
    stringi::stri_trans_general("Latin-ASCII") # Transliterate accented characters e.g. ü to U

  case_when(
    normalized %in% c("SP-GR-UA", "SP, G U U") ~ "SP_GRUENE_UN", # The 2016/2020 joint SP-Greens-independent list
    normalized == "SP" ~ "SP",                                   # In 2024 the list is SP alone
    normalized == "DIE MITTE" ~ "DIEMITTE",                      # Standardize the post-CVP/BDP party label
    normalized == "EVP - EDU" ~ "EVP_EDU",                       
    normalized %in% c("WOP", "WAHLZETTEL OHNE PARTEIBEZEICHNUNG") ~ NA_character_, # Exclude empty votes (WOP) ; this follows the Cantons Schwyz treatment
    TRUE ~ normalized |>
      str_replace_all("[^A-Z0-9]+", "_") |>
      str_remove("^_+") |>
      str_remove("_+$") # Convert any remaining label to the common identifier format
  )
}

# Compute HB seat allocation based on the former law 
# (Urteil des Bundesgerichts 1C_407/2011, 1C_445/2011 und 1C_447/2011 vom 19. März 2012 E. 5.4; referring to § 15 Ziff. 1–2 KRWG/SZ)
summarize_sz_hb <- function(district_results) {
  district_results |>
    group_by(district) |> # Districts equal municipalities
    group_modify(function(data, key) {
      allocation <- allocate_hb_with_notes(
        data$party_votes,
        first(data$district_seats),
        data$parties
      )

      bind_cols(data, allocation) |>
        mutate(
          hb_tie_note = if_else(
            is.na(hb_tie_note),
            NA_character_,
            paste0("Municipality ", key$district, ": ", hb_tie_note)
          )
        )
    }) |> # Allocate each district's seats by HB and retain unresolved ties
    ungroup() |>
    group_by(parties) |> # Combine the district HB results at cantonal level
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

# Read the Schwyz 2016 election files
read_sz_2016 <- function(source_dir, file_names) {
  source_files <- file.path(source_dir, file_names) 
  stopifnot(length(source_files) == 30L, all(file.exists(source_files))) 

  raw <- map_dfr(source_files, function(file_path) {
    read_delim(
      file_path,                                         
      delim = ";",
      col_types = cols(.default = col_character()),      
      locale = locale(encoding = "UTF-8"),
      trim_ws = TRUE,
      show_col_types = FALSE
    ) |>
      mutate(
        district = basename(file_path) |>
          tools::file_path_sans_ext() |>
          str_remove(regex("^SZ_16_", ignore_case = TRUE)) # Derive the district-name from its file name
      )
  })

  required_columns <- c("ListCode", "StimmenTotal", "StimmenProz", "Sitze") 
  stopifnot(all(required_columns %in% names(raw))) 

  district_results <- raw |>
    transmute(
      district,
      parties = standardize_sz_party(ListCode),
      party_votes = as_sz_number(StimmenTotal),          
      seats_dp = replace_na(as.integer(as_sz_number(Sitze)), 0L) 
    ) |>
    filter(!is.na(parties)) |>                 # Exclude empty votes (WoP); candidate votes are already included in party totals
    group_by(district) |>
    mutate(district_seats = sum(seats_dp)) |>  # The municipal seat total equals district magnitude
    ungroup()

  # Calculate vote shares from unrounded municipality-level voter equivalents
  vote_shares <- district_results |>
  mutate(
    voter_number = party_votes / district_seats # Voter equivalents; adjust for differing numbers of available votes per ballot
  ) |>
  group_by(parties) |>
  summarise(
    voter_number = sum(voter_number),
    .groups = "drop"
  ) |>
  mutate(
    vote_share = 100 * voter_number / sum(voter_number)
  ) |>
  select(parties, vote_share)

  official_results <- district_results |>
    group_by(parties) |>
    summarise(seats_dp = sum(seats_dp), .groups = "drop") |> # Sum the official municipality allocations by party
    transmute(canton = "SZ", year = 2016L, party = parties, seats_dp)

  final <- finalize_election_data( 
    vote_shares |> left_join(summarize_sz_hb(district_results), by = "parties"), # Join vote shares to HB seats
    official_results,
    "SZ",
    2016
  )

  district_totals <- district_results |> distinct(district, district_seats) # Count every district magnitude once
  stopifnot(
    nrow(district_totals) == 30L,
    sum(district_totals$district_seats) == 100L,
    nrow(final) == n_distinct(district_results$parties),
    any(grepl('/', final$seats_hb, fixed = TRUE)) || # With ties, party alternatives can't be summed independently;
      sum(as.integer(final$seats_hb)) == 100L,       # complete allocations are validatend in the shared HB Allocator
    sum(final$seats_dp) == 100L
  )

  final
}

# Read and process the 2020 election
read_sz_2020 <- function(source_dir, file_name) {
  raw <- read_delim(
    file.path(source_dir, file_name),            
    delim = ";",
    col_types = cols(.default = col_character()),        # Import all columns as characters
    locale = locale(encoding = "UTF-8"),
    trim_ws = TRUE,
    show_col_types = FALSE
  )

  required_columns <- c(
    "entity_name", "election_mandates", "entity_accounted_votes",
    "list_name", "list_number_of_mandates", "list_votes", "candidate_elected"
  )
  stopifnot(all(required_columns %in% names(raw)))

  source_structure <- raw |>
    group_by(entity_name, list_name) |>
    summarise(
      vote_values = n_distinct(list_votes),               # Each repeated candidate row must report the same list votes
      magnitude_values = n_distinct(election_mandates),   # Each list must repeat one district magnitude
      accounted_values = n_distinct(entity_accounted_votes),
      mandate_values = n_distinct(list_number_of_mandates),
      .groups = "drop"
    )
  stopifnot(all(source_structure[c("vote_values", "magnitude_values", "accounted_values", "mandate_values")] == 1L))

  # List totals repeat on candidate rows; retain each municipality-list total once
  district_results <- raw |>
    group_by(entity_name, list_name) |>
    summarise(
      district_seats = as.integer(as_sz_number(first(election_mandates))), 
      accounted_votes = as_sz_number(first(entity_accounted_votes)),      
      party_votes = as_sz_number(first(list_votes)),                     
      reported_seats_dp = as.integer(as_sz_number(first(list_number_of_mandates))),
      .groups = "drop"
    ) |>
    transmute(
      district = entity_name,
      district_seats = district_seats,
      accounted_votes = accounted_votes,
      parties = standardize_sz_party(list_name),        
      party_votes = party_votes,
      reported_seats_dp = reported_seats_dp
    )
  
  # Check if vote and seat totals match
  district_checks <- district_results |>
    group_by(district) |>
    summarise(
      magnitude_values = n_distinct(district_seats),
      accounted_values = n_distinct(accounted_votes),
      district_seats = first(district_seats),
      accounted_votes = first(accounted_votes),
      party_votes = sum(party_votes),
      .groups = "drop"
    )
  stopifnot(
    all(district_checks$magnitude_values == 1L),
    all(district_checks$accounted_values == 1L),
    all(district_checks$party_votes == district_checks$accounted_votes) 
  )

  total_accounted_votes <- sum(district_checks$accounted_votes) 

  # Calculate vote shares from unrounded municipality-level voter equivalents
  vote_shares <- district_results |>
  mutate(
    voter_number = party_votes / district_seats # Voter equivalents ; adjust for differing numbers of available votes per ballot
  ) |>
  group_by(parties) |>
  summarise(
    voter_number = sum(voter_number),
    .groups = "drop"
  ) |>
  mutate(
    vote_share = 100 * voter_number / sum(voter_number)
  ) |>
  select(parties, vote_share)

  elected_results <- raw |>
    transmute(
      parties = standardize_sz_party(list_name),
      elected = str_to_upper(str_squish(candidate_elected)) == "TRUE" 
    ) |>
    filter(elected) |>
    count(parties, name = "seats_dp")

  official_results <- district_results |>
    distinct(parties) |>
    left_join(elected_results, by = "parties") |>
    transmute(
      canton = "SZ",
      year = 2020L,
      party = parties,
      seats_dp = replace_na(as.integer(seats_dp), 0L)
    )

  reported_results <- district_results |>
    group_by(parties) |>
    summarise(seats_dp_reported = sum(reported_seats_dp), .groups = "drop") 
  dp_check <- official_results |>
    left_join(reported_results, by = c("party" = "parties")) # Compare candidate counts with reported list mandates

  final <- finalize_election_data(
    vote_shares |> left_join(summarize_sz_hb(district_results), by = "parties"), # Run HB within each municipality/district
    official_results,
    "SZ",
    2020
  )

  # Check if all cantonal totals match 
  stopifnot(
    nrow(district_checks) == 30L,
    sum(district_checks$district_seats) == 100L,
    sum(district_results$party_votes) == total_accounted_votes,
    all(dp_check$seats_dp == dp_check$seats_dp_reported),
    nrow(final) == n_distinct(district_results$parties),
    any(grepl('/', final$seats_hb, fixed = TRUE)) ||
      sum(as.integer(final$seats_hb)) == 100L,
    sum(final$seats_dp) == 100L
  )

  final
}

# Read and process the 2024 election
read_sz_2024 <- function(source_dir, file_name) {
  raw <- read_excel(
    file.path(source_dir, file_name),                     
    sheet = "Sitzverteilung",
    col_names = FALSE,
    na = c("", "..."),
    .name_repair = ~ paste0("x", seq_along(.x))
  )

  party_rows <- 9:14                                     # Rows 9:14 contain the six cantonal list groups
  parties <- standardize_sz_party(raw[[2]][party_rows])  
  vote_shares <- tibble(
    parties,
    vote_share = as_sz_number(raw[[4]][party_rows])      
  )
  upper_results <- tibble(
    party = parties,
    seats_dp_upper = as.integer(as_sz_number(raw[[7]][party_rows])) 
  )

  block_first_rows <- c(9L, 22L, 35L, 51L, 64L, 77L, 93L, 106L) # First party row in each municipality block
  district_vote_columns <- c(8L, 11L, 14L, 17L)                 

  district_results <- map_dfr(block_first_rows, function(first_party_row) {
    map_dfr(district_vote_columns, function(vote_column) {
      seat_label <- as.character(raw[[vote_column]][first_party_row - 2L]) # Seat magnitude sits two rows above party data
      if (is.na(seat_label) || !str_detect(seat_label, regex("Sitz", ignore_case = TRUE))) return(tibble()) # Skip empty slots

      block_parties <- standardize_sz_party(raw[[2]][first_party_row:(first_party_row + 5L)]) # Re-read the block labels
      stopifnot(identical(parties, block_parties)) # Every block must preserve the six cantonal list groups

      tibble(
        district = raw[[vote_column]][first_party_row - 3L] |> as.character() |> str_squish(), # Municipality header
        district_seats = as.integer(str_extract(seat_label, "\\d+")), # Parse values such as "9 Sitze".
        parties = block_parties,
        party_votes = replace_na(as_sz_number(raw[[vote_column]][first_party_row:(first_party_row + 5L)]), 0), # Blank party-vote and mandate cells are interpreted as zero
        seats_dp = replace_na(as.integer(as_sz_number(raw[[vote_column + 2L]][first_party_row:(first_party_row + 5L)])), 0L) 
      )
    })
  })

  # Check that each municipality's official seats equal its district magnitude
  district_checks <- district_results |>
    group_by(district) |>
    summarise(
      magnitude_values = n_distinct(district_seats),
      district_seats = first(district_seats),
      allocated_seats = sum(seats_dp),
      .groups = "drop"
    )

  stopifnot(
    all(district_checks$magnitude_values == 1L),
    all(district_checks$allocated_seats == district_checks$district_seats)
  )

  official_results <- district_results |>
    group_by(parties) |>
    summarise(seats_dp = sum(seats_dp), .groups = "drop") |> 
    transmute(canton = "SZ", year = 2024L, party = parties, seats_dp)

  dp_check <- official_results |>
    left_join(upper_results, by = "party") # Reconcile J/M/P/S totals with the cantonal allocation in G

  # Call the finalizer for the 2024 election
  final <- finalize_election_data(
    vote_shares |> left_join(summarize_sz_hb(district_results), by = "parties"), # Apply HB to municipality votes
    official_results,
    "SZ",
    2024
  )

  # Check if results match cantonal totals
  district_totals <- district_results |> distinct(district, district_seats) # Count every municipality/district once
  stopifnot(
    !anyDuplicated(parties),
    nrow(district_totals) == 30L,
    sum(district_totals$district_seats) == 100L,
    all(dp_check$seats_dp == dp_check$seats_dp_upper),
    nrow(final) == length(parties),
    any(grepl('/', final$seats_hb, fixed = TRUE)) ||
      sum(as.integer(final$seats_hb)) == 100L,
    sum(final$seats_dp) == 100L
  )

  final
}

process_canton_SZ <- function() {
  sz_16_final <- read_sz_2016(sz_source_dir, sz_source_files$election_2016)         # Use all SZ_16 municipality files
  sz_20_final <- read_sz_2020(sz_source_dir, sz_source_files$election_2020)         # Use SZ_20_kandidierendenresultate
  sz_24_final <- read_sz_2024(sz_source_dir, sz_source_files$election_2024)         # Use SZ_24_kantonsprotokoll

  bind_rows(sz_16_final, sz_20_final, sz_24_final) # Return the common election-party schema for all three years.
}