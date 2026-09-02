# Shared functions for all cantons
# Used by canton_AG.R, canton_SZ.R, canton_VS.R, etc.

# ==============================================================================
# Hagenbach-Bischoff Allocation Algorithm
# ==============================================================================
# Applies the Hagenbach-Bischoff method to allocate seats based on party votes
# Compatible with Swiss cantonal election systems (e.g., Aargau 2005 system)
#
# Args:
#   votes: numeric vector of party votes
#   n_seats: integer, number of seats to allocate
#
# Returns:
#   integer vector of allocated seats per party
allocate_hb <- function(votes, n_seats) {
  # Distribution number: next integer above valid votes / (seats + 1)
  distribution_number <- floor(sum(votes) / (n_seats + 1)) + 1
  
  # Initial seat allocation using integer quotients
  seats <- floor(votes / distribution_number)

  # Allocate remaining seats using largest quotient method
  while (sum(seats) < n_seats) {
    quotients <- votes / (seats + 1)
    winner <- which.max(quotients)
    seats[winner] <- seats[winner] + 1L
  }

  as.integer(seats)
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
#
# Returns:
#   tibble with standardized columns: 
#   election_id, parties, vote_share, small_party, seats_hb, seats_dp
finalize_election_data <- function(hb_results, dp_results, canton, year) {
  
  election_id <- paste0(canton, "_", sprintf("%02d", year %% 100))
  
  # Merge HB results with official DP results
  result <- hb_results |>
    left_join(
      dp_results |> filter(canton == !!canton, year == !!year) |> 
        select(party, seats_dp),
      by = c("parties" = "party")
    ) |>
    mutate(
      election_id = !!election_id,
      vote_share = round(vote_share, 4),
      small_party = as.integer(vote_share < 10),
      seats_hb = as.integer(seats_hb),
      seats_dp = as.integer(seats_dp)
    ) |>
    select(
      election_id, parties, vote_share, small_party, seats_hb, seats_dp
    ) |>
    arrange(desc(vote_share))
  
  # Validation checks
  stopifnot(
    !anyNA(result$seats_hb),
    !anyNA(result$seats_dp),
    abs(sum(result$vote_share) - 100) < 0.01
  )
  
  return(result)
}
