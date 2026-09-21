# Shared functions for all cantons
# Used by canton_AG.R, canton_SZ.R, canton_VS.R, etc.

# ==============================================================================
# Hagenbach-Bischoff Allocation Algorithm
# ==============================================================================
# Applies the Hagenbach-Bischoff method to allocate seats based on party votes
# Compatible with Swiss cantonal election systems (e.g., Aargau 2005 system)
#
# Exact quotient ties are detected by integer cross-multiplication rather than
# floating-point equality. Because cantonal laws can prescribe different
# tie-breakers, `tie_resolver` may narrow the tied indices according to the
# applicable law. If more than one legally possible winner remains, every
# possible final allocation is retained instead of silently choosing the first
# row. A hypothetical drawing of lots is therefore never invented.
# `quota_rule` allows cantons whose law rounds an exact quota up differently.

validate_hb_inputs <- function(votes, n_seats, party_names) {
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
    length(party_names) != length(votes) || anyNA(party_names) ||
      any(party_names == "") || anyDuplicated(party_names)
  ) {
    stop("`party_names` must contain one unique, non-empty label per vote total.")
  }
  if (n_seats > 0L && (length(votes) == 0L || sum(votes) == 0)) {
    stop("At least one party must have a positive vote total.")
  }
}

hb_allocation_outcomes <- function(
    votes,
    n_seats,
    party_names = names(votes),
    tie_resolver = NULL,
    max_outcomes = 1000L,
    quota_rule = c("floor_plus_one", "ceiling")
) {
  votes <- as.numeric(votes)
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
    return(matrix(integer(length(votes)), nrow = 1L))
  }

  raw_quota <- sum(votes) / (n_seats + 1L)
  distribution_number <- if (quota_rule == "ceiling") {
    ceiling(raw_quota)
  } else {
    floor(raw_quota) + 1L
  }
  initial_seats <- as.integer(floor(votes / distribution_number))
  if (sum(initial_seats) > n_seats) {
    stop("The initial HB allocation exceeds the number of available seats.")
  }

  states <- matrix(initial_seats, nrow = 1L)
  while (sum(states[1L, ]) < n_seats) {
    next_states <- lapply(seq_len(nrow(states)), function(state_index) {
      state <- states[state_index, ]
      provisional <- which.max(votes / (state + 1L))
      tied <- which(
        votes * (state[provisional] + 1L) ==
          votes[provisional] * (state + 1L)
      )

      admissible <- tied
      if (length(tied) > 1L && is.function(tie_resolver)) {
        admissible <- tie_resolver(
          tied = tied,
          votes = votes,
          seats = state,
          distribution_number = distribution_number,
          party_names = party_names
        )
      }
      if (
        length(admissible) == 0L || anyNA(admissible) ||
          any(admissible != as.integer(admissible)) ||
          any(!admissible %in% tied)
      ) {
        stop("`tie_resolver` must return one or more indices from `tied`.")
      }

      do.call(rbind, lapply(as.integer(admissible), function(winner) {
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

  storage.mode(states) <- "integer"
  colnames(states) <- party_names
  states
}

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
    votes,
    n_seats,
    party_names,
    tie_resolver,
    quota_rule = quota_rule
  )

  possible_seats <- lapply(seq_along(party_names), function(index) {
    sort(unique(outcomes[, index]))
  })
  ambiguous <- lengths(possible_seats) > 1L
  seats_hb <- vapply(possible_seats, function(values) {
    if (length(values) == 1L) values else NA_integer_
  }, integer(1L))
  hb_tie_note <- rep(NA_character_, length(party_names))

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
      "Unresolved exact HB tie; no hypothetical draw was imposed. ",
      "Possible final allocations: ",
      paste(alternatives, collapse = " or "),
      "."
    )
    hb_tie_note[ambiguous] <- note
  }

  tibble::tibble(
    seats_hb = seats_hb,
    seats_hb_options = vapply(
      possible_seats,
      paste,
      collapse = "/",
      FUN.VALUE = character(1L)
    ),
    hb_tie_note = hb_tie_note
  )
}

# Combine a party's possible district results into possible canton-wide totals.
# For example, c("1", "2/3", "8") becomes "11/12".
combine_hb_seat_options <- function(options) {
  if (length(options) == 0L || anyNA(options)) {
    stop("`options` must contain one non-missing HB option per district.")
  }

  possible_totals <- 0L
  for (option in options) {
    values <- suppressWarnings(
      as.integer(strsplit(option, "/", fixed = TRUE)[[1L]])
    )
    if (length(values) == 0L || anyNA(values) || any(values < 0L)) {
      stop("Each HB option must contain non-negative integers separated by '/'.")
    }
    possible_totals <- sort(unique(
      as.integer(outer(possible_totals, values, "+"))
    ))
  }

  paste(possible_totals, collapse = "/")
}

unique_hb_seat_value <- function(options) {
  values <- suppressWarnings(
    as.integer(strsplit(options, "/", fixed = TRUE)[[1L]])
  )
  if (length(values) == 1L && !is.na(values)) values else NA_integer_
}

# Returns the usual integer vector when the allocation is unique. If an exact
# tie can change the final allocation, fail explicitly so callers cannot mistake
# row order for a legal tie-break. Call `allocate_hb_with_notes()` when the
# ambiguity should be retained in the output data.
allocate_hb <- function(
    votes, n_seats, tie_resolver = NULL,
    quota_rule = c("floor_plus_one", "ceiling")
) {
  result <- allocate_hb_with_notes(
    votes,
    n_seats,
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
# Standardizes and validates the final cleaned election dataset
#
# Args:
#   hb_results: tibble with party, votes, seats_hb
#   dp_results: tibble with official seats_dp
#   canton: character, canton abbreviation (e.g., "AG")
#   year: numeric, election year
#   parliament_size: total seats in parliament; defaults to the analyzed DP seats
#
# Returns:
#   tibble with standardized columns: 
#   election_id, parliament_size, seats_allocated_dp, parties, vote_share,
#   small_party, seats_hb, seats_dp
finalize_election_data <- function(
    hb_results, dp_results, canton, year, parliament_size = NULL
) {
  
  election_id <- paste0(canton, "_", sprintf("%02d", year %% 100))
  
  # Merge HB results with official DP results
  if (!"hb_tie_note" %in% names(hb_results)) {
    hb_results <- hb_results |>
      mutate(hb_tie_note = NA_character_)
  }
  if (!"seats_hb_options" %in% names(hb_results)) {
    hb_results <- hb_results |>
      mutate(seats_hb_options = as.character(seats_hb))
  }

  result <- hb_results |>
    left_join(
      dp_results |> filter(canton == !!canton, year == !!year) |> 
        select(party, seats_dp),
      by = c("parties" = "party")
    ) |>
    mutate(
      election_id = !!election_id,
      seats_allocated_dp = sum(seats_dp),
      parliament_size = if (is.null(.env$parliament_size)) {
        seats_allocated_dp
      } else {
        as.integer(.env$parliament_size)
      },
      vote_share = round(vote_share, 4),
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
