# Schaffhausen cantonal elections, 2008-2024. 
# The election results are in contrarty to all other cantons only published in pdf form 
# and could also not be received in xlsx or csv form through mail contact.

library(tidyverse) # Tools for tables, text, and loops.
library(here)      # Build paths from the project folder.

here::i_am("data_cleaning_cantons/canton_SH.R") # Fix the project root.

if (!exists("allocate_hb_with_notes", mode = "function") ||
    !exists("combine_hb_seat_options", mode = "function") ||
    !exists("finalize_election_data", mode = "function")) {
  source(here("data_cleaning_cantons", "00_functions.R")) # Load shared seat functions.
}

# (Kanton Schaffhausen, n.d.; Staatskanzlei Kanton Schaffhausen, 2020, 2024)
sh_source_dir <- here("data", "datasets", "schaffhausen") # Folder with the PDFs.
sh_source_files <- tibble( # Match each election year to its PDF.
  year = c(2008L, 2012L, 2016L, 2020L, 2024L),
  file = paste0("SH_", c("08", "12", "16", "20", "24"), "_kantonsratswahlen.pdf")
)

sh_pdftotext <- function() {
  executable <- Sys.which("pdftotext") # Look for the PDF text tool on PATH.
  if (nzchar(executable)) return(executable) # Use it if found.

  git_executable <- file.path(
    Sys.getenv("ProgramFiles"), "Git", "mingw64", "bin", "pdftotext.exe"
  ) # Git for Windows may include its own copy.
  if (file.exists(git_executable)) return(git_executable) # Use that copy if found.
  stop("Install Poppler's pdftotext command (or Git for Windows) to read the SH PDFs.") # Stop if absent.
}

sh_pdf_pages <- function(path) {
  if (!file.exists(path)) stop("Missing Schaffhausen PDF: ", path) # Require the file.
  output <- suppressWarnings(system2(
    sh_pdftotext(), # Extract the PDF text in its original reading order.
    c("-raw", "-enc", "UTF-8", shQuote(normalizePath(path, winslash = "/")), "-"),
    stdout = TRUE,
    stderr = TRUE
  ))
  if (!is.null(attr(output, "status")) || length(output) == 0L) {
    stop("pdftotext could not read ", path, ": ", paste(output, collapse = " "))
  }
  strsplit(paste(output, collapse = "\n"), "\f", fixed = TRUE)[[1L]] # Split into pages.
}

sh_lines <- function(page) {
  lines <- str_squish(strsplit(page, "\n", fixed = TRUE)[[1L]]) # Clean spaces.
  lines[nzchar(lines)] # Drop blank lines.
}

sh_number <- function(value) {
  as.numeric(str_remove_all(value, "['’ ]")) # Remove Swiss thousands marks.
}

standardize_sh_party <- function(label, year, list_number) {
  normalized <- label |> # Make party names comparable across PDFs.
    str_to_upper() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_squish()
  if (year %in% c(2008L, 2012L, 2016L) && list_number == "05") {
    return("OEBS") # List 05 is OeBS in these years.
  }
  case_when( # Merge spelling variants under one party code.
    normalized %in% c("GRUNE", "GRUENE") ~ "GRUENE",
    normalized %in% c("DIE MITTE", "MITTE") ~ "DIEMITTE",
    normalized %in% c("SVP/SEN.", "SVP-S", "SVP/SEN") ~ "SVP_SEN",
    normalized %in% c("SVP/AGRO", "SVP-A") ~ "SVP_AGRO",
    normalized %in% c("SVP/KMU", "SVP-K") ~ "SVP_KMU",
    normalized %in% c("JF", "JFSH") ~ "JFSH",
    normalized %in% c("JUSOPLUS", "JUSO") ~ "JUSO",
    TRUE ~ normalized |>
      str_replace_all("[^A-Z0-9]+", "_") |>
      str_remove("^_+|_+$")
  )
}

sh_list_rows <- function(lines, label) {
  positions <- which(str_detect(lines, "^[0-9]{2} .+")) # Find numbered lists.
  if (length(positions) == 0L) stop("No party rows found on ", label)
  tibble(
    position = positions,
    list_number = str_extract(lines[positions], "^[0-9]{2}"), # Keep the list ID.
    list_label = str_remove(lines[positions], "^[0-9]{2} ") # Keep its name.
  )
}

read_sh_upper <- function(page, year) {
  lines <- sh_lines(page) # Read the Oberzuteilung page line by line.
  if (!any(lines == "Oberzuteilung") || !any(lines == "Sitzanspruch")) {
    stop(year, ": missing Oberzuteilung table")
  }
  seat_position <- which(str_detect(lines, "^Sitze Gesamt")) # Find parliament size.
  if (length(seat_position) != 1L) stop(year, ": missing parliament seat total")
  parliament_size <- as.integer(sh_number(lines[seat_position - 1L])) # Seats are above the label.
  rows <- sh_list_rows(lines, paste(year, "Oberzuteilung")) # Find party rows.
  if (any(rows$position < 5L)) stop(year, ": incomplete Oberzuteilung row")

  upper <- rows |> # Read the four values printed before each party name.
    mutate(
      party_votes = sh_number(lines[position - 1L]), # Raw Stimmen.
      voter_number = sh_number(lines[position - 2L]), # Weighted Wählerzahl.
      quotient = sh_number(lines[position - 3L]), # Published DP quotient.
      seats_dp = as.integer(sh_number(lines[position - 4L])), # Official DP seats.
      parties = map2_chr(list_number, list_label, # Standardize party names.
        ~ standardize_sh_party(.y, year, .x)
      )
    ) |>
    select(list_number, parties, party_votes, voter_number, quotient, seats_dp)

  if (anyNA(upper) || anyDuplicated(upper$list_number) ||
      anyDuplicated(upper$parties) || any(upper$party_votes < 0) ||
      any(upper$party_votes != floor(upper$party_votes)) ||
      sum(upper$seats_dp) != parliament_size || parliament_size != 60L) { # Check totals.
    stop(year, ": invalid Oberzuteilung party totals")
  }
  list(data = upper, parliament_size = parliament_size)
}

read_sh_district <- function(page, year) {
  lines <- sh_lines(page) # Read one Rekapitulation page.
  header <- which(lines == "Bezeichnung der Liste") # Start of list table.
  district_position <- which(str_detect(lines, "^Wahlkreis:?$")) # District label.
  total_position <- which(lines == "Total") # End of list table.
  if (length(header) != 1L || length(district_position) != 1L ||
      length(total_position) != 1L) {
    stop(year, ": incomplete Wahlkreis Rekapitulation table")
  }
  district <- lines[district_position - 1L] # Name above "Wahlkreis".
  rows <- sh_list_rows(lines, paste(year, district)) |>
    filter(position > header, position < total_position) # Keep table rows only.
  if (nrow(rows) == 0L || any(rows$position < 3L)) {
    stop(year, ": no rows in Wahlkreis ", district)
  }

  result <- rows |> # Read votes and seats for each district list.
    mutate(
      district = district,
      party_votes = sh_number(lines[position - 1L]), # Votes above the list name.
      seat_field = lines[position - 2L], # Seats, sometimes with a percent.
      seats_dp_district = as.integer(str_extract(seat_field, "[0-9]+$")), # DP seats.
      printed_share = if_else( # The 2008 PDF has no printed percentages.
        str_detect(seat_field, "%"),
        as.numeric(str_extract(seat_field, "^[0-9.]+")),
        NA_real_
      )
    ) |>
    select(district, list_number, party_votes, seats_dp_district, printed_share)

  reported_seats <- as.integer(sh_number(lines[total_position - 2L])) # Total district seats.
  reported_votes <- sh_number(lines[total_position - 1L]) # Total district votes.
  result <- result |>
    mutate(
      district_seats = reported_seats,
      district_vote_share = 100 * party_votes / reported_votes # Share within district.
    )
  if (anyNA(result[c("party_votes", "seats_dp_district", "district_seats")]) ||
      anyDuplicated(result$list_number) ||
      any(result$party_votes < 0) ||
      any(result$party_votes != floor(result$party_votes)) ||
      sum(result$party_votes) != reported_votes ||
      sum(result$seats_dp_district) != reported_seats ||
      any(abs(result$district_vote_share - result$printed_share) > 0.011,
           na.rm = TRUE)) { # Compare with printed percentages where available.
    stop(year, ": inconsistent Rekapitulation for Wahlkreis ", district)
  }
  result
}

read_sh_election <- function(path, year) {
  pages <- sh_pdf_pages(path) # Read one election PDF.
  upper_pages <- which(vapply(pages, function(page) { # Find Oberzuteilung.
    any(sh_lines(page) == "Oberzuteilung")
  }, logical(1L)))
  district_pages <- which(vapply(pages, function(page) { # Find the six list tables.
    lines <- sh_lines(page)
    any(lines == "Bezeichnung der Liste") &&
      any(str_detect(lines, "^Wahlkreis:?$")) &&
      any(lines == "Rekapitulation")
  }, logical(1L)))
  if (length(upper_pages) != 1L || length(district_pages) != 6L) {
    stop(year, ": expected one Oberzuteilung and six Wahlkreis Rekapitulation pages")
  }

  upper <- read_sh_upper(pages[upper_pages], year) # Official canton-level results.
  districts <- map_dfr(pages[district_pages], read_sh_district, year = year) |>
    left_join(upper$data |> select(list_number, parties), by = "list_number") # Match list IDs.
  district_totals <- districts |>
    distinct(district, district_seats) # Count each district's seats once.
  vote_checks <- districts |> # Rebuild canton totals from district rows.
    group_by(list_number) |>
    summarise(
      party_votes_district = sum(party_votes), # Raw party votes.
      voter_number_reconstructed = sum(party_votes / district_seats), # Weighted votes.
      seats_dp_district = sum(seats_dp_district), # Official district seats.
      .groups = "drop"
    ) |>
    right_join(upper$data, by = "list_number")
  voter_tolerance <- if (year <= 2012L) 3 else if (year <= 2020L) 0.01 else 0.0001 # Allow PDF rounding.

  if (n_distinct(districts$district) != 6L ||
      anyNA(districts$parties) ||
      nrow(district_totals) != 6L ||
      sum(district_totals$district_seats) != upper$parliament_size ||
      anyNA(vote_checks[c("party_votes_district", "seats_dp_district")]) ||
      any(vote_checks$party_votes_district != vote_checks$party_votes) ||
      any(abs(vote_checks$voter_number_reconstructed - vote_checks$voter_number) >
            voter_tolerance) ||
      any(vote_checks$seats_dp_district != vote_checks$seats_dp)) { # Require matching totals.
    stop(year, ": district results do not reconcile with Oberzuteilung")
  }

  hb_by_district <- districts |> # Former HB rule: Regierungsrat des Kantons Schaffhausen (2007, p. 6).
    group_by(district) |> # Allocate each Wahlkreis separately.
    group_modify(function(data, key) { # Report gives no exact HB tie rule; old law requested, no reply yet.
      allocation <- allocate_hb_with_notes( # Keep all outcomes if an exact tie occurs.
        data$party_votes,
        first(data$district_seats),
        data$parties
      ) # None of the 30 district allocations in these five PDFs has a tie.
      bind_cols(data, allocation) |>
        mutate(hb_tie_note = if_else( # Name the district if a tie is found.
          is.na(hb_tie_note), NA_character_,
          paste0("Wahlkreis ", key$district, ": ", hb_tie_note)
        ))
    }) |>
    ungroup()

  hb_results <- hb_by_district |> # Add each party's district HB seats.
    group_by(parties) |>
    summarise(
      seats_hb_options = combine_hb_seat_options(seats_hb_options), # Keep tie options.
      hb_tie_note = {
        notes <- unique(na.omit(hb_tie_note))
        if (length(notes) == 0L) NA_character_ else paste(notes, collapse = " | ")
      },
      .groups = "drop"
    )

  
  vote_shares <- upper$data |>
    mutate(vote_share = 100 * voter_number / sum(voter_number)) |> # Use weighted votes.
    select(parties, vote_share)
  official_results <- upper$data |>
    transmute(canton = "SH", year = year, party = parties, seats_dp) # Official DP seats.
  final <- finalize_election_data( # Build the common output table.
    left_join(vote_shares, hb_results, by = "parties"),
    official_results,
    "SH",
    year,
    parliament_size = upper$parliament_size
  )
  if (nrow(final) != nrow(upper$data) ||
      sum(final$seats_dp) != upper$parliament_size ||
      (!any(str_detect(final$seats_hb, "/")) &&
        sum(as.integer(final$seats_hb)) != upper$parliament_size)) { # Check both seat totals.
    stop(year, ": invalid final Schaffhausen seat totals")
  }
  final
}

process_canton_SH <- function() {
  map2_dfr( # Read all five PDFs and stack their results.
    file.path(sh_source_dir, sh_source_files$file),
    sh_source_files$year,
    read_sh_election
  )
}

options(width = 200) # Temporary check: fit all columns in the terminal.
print(process_canton_SH(), n = Inf, width = 200) # Review every party row.
