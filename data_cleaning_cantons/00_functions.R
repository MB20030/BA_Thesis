# Shared functions for all cantons
# Used by canton_AG.R, canton_SZ.R, canton_VS.R, etc.

# ==============================================================================
# Hagenbach-Bischoff Allocation Algorithm
# ==============================================================================
# Applies the Hagenbach-Bischoff method to allocate seats based on party votes
# General HB allocator with configurable quota and tie-breaking rules.
# Canton-specific eligibility, alliances and candidate limits require
# separate implementation where applicable.
# Quotient ties are checked by cross-multiplication.
# Because cantonal laws can prescribe different tie-breakers, `tie_resolver` 
# may narrow the tied indices according to the applicable law. 
# If more than one legally possible winner remains, every
# possible final allocation is retained.
# `quota_rule` allows cantons whose law rounds an exact quota up differently.

library(dplyr)

validate_hb_inputs <- function(votes, n_seats, party_names) { # Defines the validator
  if (  # Starts and invalid-input check
    !is.numeric(votes) || any(!is.finite(votes)) || any(votes < 0) ||
      any(votes != floor(votes)) # Check if votes have appropriate type, direction...
  ) { 
    stop("`votes` must be a numeric vector of non-negative finite whole numbers.")
  } # explicit error message
  if (
    length(n_seats) != 1L || !is.numeric(n_seats) || !is.finite(n_seats) ||
      n_seats < 0 || n_seats != floor(n_seats) || n_seats > 180L # Validate number of seats
  ) {
    stop("`n_seats` must be one non-negative integer and can be maximum 180 (canton of Zurich).")
  }
  if (
    length(party_names) != length(votes) || anyNA(party_names) ||
    any(!nzchar(trimws(party_names))) || anyDuplicated(party_names)
  ) {
    stop("`party_names` must contain one unique, non-empty label per vote total.")
  }
  if (n_seats > 0L && (length(votes) == 0L || sum(votes) == 0)) {
    stop("At least one party must have a positive vote total.")
  }
}

# Define the main allocation function
hb_allocation_outcomes <- function(
    votes,
    n_seats,
    party_names = names(votes),
    tie_resolver = NULL,
    max_outcomes = 1000L,
    quota_rule = c("floor_plus_one", "ceiling")
) {
  if (is.null(party_names)) {
    party_names <- paste0("party_", seq_along(votes))
  }
  party_names <- as.character(party_names)
  validate_hb_inputs(votes, n_seats, party_names)
  n_seats <- as.integer(n_seats)
  quota_rule <- match.arg(quota_rule)

  if (!is.null(tie_resolver) && !is.function(tie_resolver)) {
    stop("`tie_resolver` must be NULL or a function.")
  }
  if (
    length(max_outcomes) != 1L || !is.numeric(max_outcomes) ||
      !is.finite(max_outcomes) || max_outcomes < 1L ||
      max_outcomes != floor(max_outcomes)
  ) {
    stop("`max_outcomes` must be one positive integer.")
  }

  if (n_seats == 0L) {
    return(matrix(integer(length(votes)), nrow = 1L, dimnames = list(NULL, party_names)))
  }

  # Hagenbach-Bischoff allocation process
  raw_quota <- sum(votes) / (n_seats + 1L) # Initial quota allocation
  distribution_number <- if (quota_rule == "ceiling") { # Apply the selected quota runding rule
    ceiling(raw_quota) # Rounds upward to the nearest integer
  } else {
    floor(raw_quota) + 1L # Rounds downward and adds one
  }
  initial_seats <- as.integer(floor(votes / distribution_number))
  if (sum(initial_seats) > n_seats) {
    stop("The initial HB allocation exceeds the number of available seats.")
  }
  
  # Allocation of left-over seats
  states <- matrix(initial_seats, nrow = 1L)
  while (sum(states[1L, ]) < n_seats) {
    next_states <- lapply(seq_len(nrow(states)), function(state_index) {
      state <- states[state_index, ]
      provisional <- which.max(votes / (state + 1L)) # Find eligible recipient for seat: highest quotient
      tied <- which( # Identify all tied maxima
        votes * (state[provisional] + 1L) ==
          votes[provisional] * (state + 1L)
      )
      # Apply the canton specific tie rule
      admissible <- tied
      if (length(tied) > 1L && is.function(tie_resolver)) { # Call the tie resolver if needed and available
        admissible <- tie_resolver(
          tied = tied,
          votes = votes,
          seats = state,
          distribution_number = distribution_number,
          party_names = party_names
        )
      }
      if (
        !is.numeric(admissible) || length(admissible) == 0L ||
          any(!is.finite(admissible)) ||
          any(admissible != floor(admissible)) ||
          any(!admissible %in% tied)
      ) {
        stop("`tie_resolver` must return one or more indices from `tied`.")
      }

      admissible <- unique(as.integer(admissible))

      do.call(rbind, lapply(admissible, function(winner) {
        outcome <- state
        outcome[winner] <- outcome[winner] + 1L
        outcome
      }))
    })

    states <- unique(do.call(rbind, next_states))
    if (nrow(states) > max_outcomes) {
      stop(
        "HB tie branching exceeded `max_outcomes`; inspect the tied allocation manually."
      )
    }
  }
  # Return the final allocation matrix
  storage.mode(states) <- "integer"
  colnames(states) <- party_names
  states
}
# Define the user-facing allocation wrapper
allocate_hb_with_notes <- function(
    votes,
    n_seats,
    party_names = names(votes),
    tie_resolver = NULL,
    quota_rule = c("floor_plus_one", "ceiling")
) {
  if (is.null(party_names)) {
    party_names <- paste0("party_", seq_along(votes))
  }
  outcomes <- hb_allocation_outcomes(
    votes = votes,
    n_seats = n_seats,
    party_names = party_names,
    tie_resolver = tie_resolver,
    quota_rule = quota_rule
  )
  # Summarize each party's possible seats
  possible_seats <- lapply(seq_along(party_names), function(index) {
    sort(unique(outcomes[, index]))
  })
  ambiguous <- lengths(possible_seats) > 1L # Identify parties with multiple possible seat allocations
  seats_hb <- vapply(possible_seats, function(values) {
    if (length(values) == 1L) values else NA_integer_
  }, integer(1L))
  hb_tie_note <- rep(NA_character_, length(party_names))
  
  #  Note the allocation alternatives if there are any unresolved ties
  if (any(ambiguous)) {
    alternatives <- apply(
      outcomes[, ambiguous, drop = FALSE],
      1L,
      function(outcome) {
        paste0(
          party_names[ambiguous], "=", outcome,
          collapse = ", "
        )
      }
    )
    note <- paste0(
      "Unresolved exact HB tie, no hypothetical draw was imposed. ",
      "Possible final allocations: ",
      paste(alternatives, collapse = " or "),
      "."
    )
    hb_tie_note[ambiguous] <- note
  }

  # Return the party-level tibble
  tibble::tibble(
    seats_hb = seats_hb,
    seats_hb_options = vapply(
      possible_seats,
      paste,
      collapse = "/", # Separate alternatives with a slash for display
      FUN.VALUE = character(1L)
    ),
    hb_tie_note = hb_tie_note
  )
}

# Combine a party's possible district results into possible canton-wide totals.
# Thus c("1", "2/3", "8") becomes "11/12".
combine_hb_seat_options <- function(options) {
  if (length(options) == 0L || anyNA(options)) {
    stop("`options` must contain one non-missing HB option per district.")
  }

  # Check the format before converting options to integers.
  if (
    !is.character(options) ||
      any(!grepl("^[0-9]+(/[0-9]+)*$", options))
  ) {
    stop("Each HB option must contain non-negative integers separated by '/'.")
  }

  possible_totals <- 0L
  for (option in options) {
    values <- suppressWarnings(
      as.integer(strsplit(option, "/", fixed = TRUE)[[1L]])
    )
    if (length(values) == 0L || anyNA(values) || any(values < 0L)) {
      stop("HB seat options could not be converted to valid non-negative integers.")
    }
    possible_totals <- sort(unique(
      as.integer(outer(possible_totals, values, "+"))
    ))
  }

  paste(possible_totals, collapse = "/")
}

unique_hb_seat_value <- function(options) {
  # Require one correctly formatted seat-option string
  if (
    !is.character(options) || length(options) != 1L ||
      is.na(options) || !grepl("^[0-9]+(/[0-9]+)*$", options)
  ) {
    stop("`options` must be one valid HB seat-option string.")
  }

  values <- as.numeric(strsplit(options, "/", fixed = TRUE)[[1L]])

  # Check that the values can be stored as integers
  if (any(!is.finite(values)) || any(values > .Machine$integer.max)) {
    stop("HB seat options exceed the supported integer range.")
  }

  values <- unique(as.integer(values))

  # Return a definite count only, when one unique value exists
  if (length(values) == 1L) values else NA_integer_
}

# Return integer seat counts, stop if an unresolved tie allows multiple allocations.
# Use allocate_hb_with_notes() to preserve those alternatives.
allocate_hb <- function(
    votes, n_seats, tie_resolver = NULL,
    quota_rule = c("floor_plus_one", "ceiling")
) {
  result <- allocate_hb_with_notes(
    votes = votes,
    n_seats = n_seats,
    party_names = names(votes),
    tie_resolver = tie_resolver,
    quota_rule = quota_rule
  )
  if (anyNA(result$seats_hb)) {
    stop(
      result$hb_tie_note[which(!is.na(result$hb_tie_note))[1L]],
      " Use `allocate_hb_with_notes()` to retain this ambiguity in the data."
    )
  }

  as.integer(result$seats_hb)
}

# ==============================================================================
# Finalize Election Data
# ==============================================================================
# Join HB and official DP results, standardize columns, and validate the output.
# hb_results requires parties, vote_share, seats_hb.
# dp_results requires canton, year, party, seats_dp.
# parliament_size defaults to the analyzed DP seat total.

finalize_election_data <- function(
    hb_results, dp_results, canton, year, parliament_size = NULL
) {
  # Check required arguments and input columns before processing
  stopifnot(
    is.character(canton), length(canton) == 1L,
    !is.na(canton), nzchar(canton),
    is.numeric(year), length(year) == 1L,
    is.finite(year), year == floor(year),
    all(c("parties", "vote_share", "seats_hb") %in% names(hb_results)),
    all(c("canton", "year", "party", "seats_dp") %in% names(dp_results))
  )

  election_id <- paste0(canton, "_", sprintf("%02d", year %% 100))
  
  # Merge HB with DP results
  if (!"hb_tie_note" %in% names(hb_results)) {
    hb_results <- hb_results |>
      mutate(hb_tie_note = NA_character_)
  }
  if (!"seats_hb_options" %in% names(hb_results)) {
    hb_results <- hb_results |>
      mutate(seats_hb_options = as.character(seats_hb))
  }

  # Select the official results for this election.
  official <- dp_results |>
    dplyr::filter(
      .data$canton == .env$canton,
      .data$year == .env$year
    ) |>
    dplyr::select(party, seats_dp)

  # Check party matching and seat values before joining
  stopifnot(
    !anyNA(hb_results$parties),
    !anyNA(official$party),
    !anyDuplicated(hb_results$parties),
    !anyDuplicated(official$party),
    setequal(hb_results$parties, official$party),
    is.numeric(official$seats_dp),
    all(is.finite(official$seats_dp)),
    all(official$seats_dp >= 0),
    all(official$seats_dp == floor(official$seats_dp))
  )

  result <- hb_results |>
    left_join(
      official,
      by = c("parties" = "party")
    ) |>
    mutate(
      election_id = .env$election_id,
      seats_allocated_dp = sum(seats_dp),
      parliament_size = if (is.null(.env$parliament_size)) {
        seats_allocated_dp
      } else {
        as.integer(.env$parliament_size)
      },
      small_party = as.integer(vote_share < 10),
      seats_hb = as.character(seats_hb_options),
      seats_dp = as.integer(seats_dp),
      hb_tie_note = as.character(hb_tie_note)
    ) |>
    select(
      election_id, parliament_size, seats_allocated_dp, parties, vote_share,
      small_party, seats_hb, seats_dp
    ) |>
    arrange(desc(vote_share))
  
  # Validation checks
  stopifnot(
    !anyNA(result$seats_hb),
    all(grepl('^[0-9]+(?:/[0-9]+)*$', result$seats_hb)),
    !anyNA(result$seats_dp),
    abs(sum(result$vote_share) - 100) < 0.01
  )
  
  return(result)
}
