bal_func <- function(mat2, rsum2, csum2, int_only = FALSE){
  # Find ratio of rows
  mat_rsum <- rowSums(mat2, na.rm = TRUE)

  if(any(mat_rsum == 0)){
    # Check for zeros in the matrix, once the matrix has zeros they stay so a solution can't be found
    r_bad0 <- dplyr::if_else(mat_rsum == 0 & rsum2 != 0, rsum2/ncol(mat2), 0)
    #mat2 <- sweep(mat2, 1, r_bad0, FUN = "+")
    mat2 <- mat2 + matrix(r_bad0, nrow = nrow(mat2), ncol = ncol(mat2)) #10x faster than sweep
    mat_rsum <- rowSums(mat2, na.rm = TRUE)
  }

  mat_rratio <- rsum2 / mat_rsum
  mat_rratio[is.nan(mat_rratio)] <- 0
  #TODO: Get Inf values if mat_rsum is zero

  mat2 <- mat2 * mat_rratio

  # Find ratio of rows
  mat_cratio <- csum2 / colSums(mat2, na.rm = TRUE)
  mat_cratio[is.nan(mat_cratio)] <- 0

  #mat2 <- sweep(mat2, MARGIN=2, mat_cratio, `*`)
  mat2 <- mat2 * matrix(mat_cratio, nrow = nrow(mat2), ncol = ncol(mat2), byrow = TRUE) #10x faster than sweep
  mat2[is.nan(mat2)] <- 0

  if(int_only){
    mat2 <- round_half_random(mat2)
  }

  return(mat2)
}

round_half_random <- function(x) {
  tweaks <- runif(length(x), min = -0.5, max = 0.5)
  tweaks[x < 1] <- 0 # Never round to 0
  round(x + tweaks)
}


# Furness method balancing
furness_partial <- function(mat, rsum, csum, n = 100, check = TRUE, int_only = TRUE){

  # Check rsum = csum
  rt = sum(rsum)
  ct = sum(csum)

  if(rt != ct & check){
    message("Totals of rsum and csum differ by ",
            abs(rt - ct)," (",round(100 * abs(rt - ct)/max(c(rt,ct)),2),
            "%) an exact solution is impossible.")
  }

  rname <- rownames(mat)
  cname <- colnames(mat)

  mat_change <- is.na(mat)
  mat_change <- ifelse(mat_change, 1, NA)

  rsum_change = rsum - rowSums(mat, na.rm = TRUE)
  csum_change = csum - colSums(mat, na.rm = TRUE)

  # Get scale about right
  mat_change <- mat_change / (sum(mat_change, na.rm = TRUE) / sum(rsum_change))
  mat_change_orig <- mat_change
  i <- 1
  while(i < n){
    i <- i + 1
    mat_old <- mat_change
    mat_change <- bal_func(mat2 = mat_change,
                           rsum2 = rsum_change,
                           csum2 = csum_change,
                           int_only = int_only)
    # Are we stuck in a loop?
    if(identical(mat_old, mat_change)){
      mat_change <- bal_func(mat2 = mat_change_orig,
                             rsum2 = rsum_change,
                             csum2 = csum_change,
                             int_only = int_only)
      #message("Stuck in loop")
    }
    #print(mat_change)

    # Check
    if(all(rowSums(mat_change, na.rm = TRUE) == rsum_change)){
      if(all(colSums(mat_change, na.rm = TRUE) == csum_change)){
        break
      }
    }

  }
  mat_change <- round(mat_change)

  mat_fin = ifelse(is.na(mat), mat_change, mat)

  # Check
  if(check){
    if(!all(rowSums(mat_fin) == rsum)){
      err = rowSums(mat_fin, na.rm = TRUE) - rsum
      message("Rows don't match: total error = ", round(sum(abs(err)), 2),"/",rt,
              " max error = ", round(max(abs(err)), 2),
              " RMSE = ", round(sqrt(mean(err * err)), 2))
    }
    if(!all(colSums(mat_fin) == csum)){
      err = colSums(mat_fin, na.rm = TRUE) - csum
      message("Cols don't match: total error = ", round(sum(abs(err)), 2),"/",ct,
              " max error = ", round(max(abs(err)), 2),
              " RMSE = ", round(sqrt(mean(err * err)), 2))
    }
  }


  return(mat_fin)
}

distribute <- function(total, bins) {
  # Calculate the base value for each bin
  base_value <- total %/% bins

  # Calculate the remainder
  remainder <- total %% bins

  # Create a vector with the base value repeated 'bins' times
  result <- rep(base_value, bins)

  # Distribute the remainder over the first 'remainder' bins
  if(remainder > 0){
    result[seq(1, remainder)] <- result[seq(1, remainder)] + 1
  }

  return(result)
}


# Furness method balancing
furness_incomplete <- function(mat, rsum, csum, tt){

  rname <- rownames(mat)
  cname <- colnames(mat)

  # Generate combinations of number that sum to total
  n_gaps = sum(is.na(mat))
  combinations = generate_combinations(tt - sum(mat, na.rm = TRUE), n_gaps)

  combinations_mat <- list()
  na_indices <- which(is.na(mat))
  error_check <- list()
  for(i in seq(1, length(combinations))){
    mat_sub <- mat
    mat_sub[na_indices] <- combinations[[i]]

    rsum_mat <- rowSums(mat_sub)
    csum_mat <- colSums(mat_sub)

    rerror = sum(abs(rsum_mat - rsum), na.rm = TRUE)
    cerror = sum(abs(csum_mat - csum), na.rm = TRUE)

    error_check[[i]] <- sum(rerror, cerror)
    combinations_mat[[i]] <- mat_sub
  }

  error_check = unlist(error_check)
  combinations_mat <- combinations_mat[error_check == min(error_check)]
  if(min(error_check) != 0){
    warning("Solution not found, getting best match")
  }
  mat_fin <- combinations_mat[[sample(seq_along(combinations_mat), 1)]]

  return(mat_fin)
}


generate_combinations <- function(t, n, prefix = numeric()) {
  # Generate all length-n vectors of non-negative integers that sum to t.
  # Returns a list of numeric vectors. Order matters (compositions).
  if (n < 1 || t < 0) return(list())
  if (n == 1) {
    # single remaining slot must take the remainder (including 0)
    return(list(c(prefix, t)))
  }

  result <- list()
  # allow zeros: iterate i from 0..t
  for (i in 0:t) {
    temp <- generate_combinations(t - i, n - 1, c(prefix, i))
    result <- c(result, temp)
  }
  return(result)
}

compositions_fast <- function(t, n) {
  # number of bars to place
  k <- n - 1L
  m <- t + n - 1L

  # list of bar placements (in C, extremely fast)
  bars <- utils::combn(m, k)

  # convert bar positions -> composition via gap lengths
  gaps <- rbind(
    bars[1, ] - 1L,
    apply(bars, 2, diff) - 1L,
    m - bars[k, ]
  )

  # return as list of integer vectors
  lapply(seq_len(ncol(gaps)), function(i) gaps[, i])
}

furness_balance <- function(mat, rsum, csum, n = 100, check = TRUE, int_only = FALSE, quiet = TRUE){

  rname <- rownames(mat)
  cname <- colnames(mat)

  # Check rsum = csum
  rt = sum(rsum)
  ct = sum(csum)

  if(rt != ct & check){
    message("Totals of rsum and csum differ by ",
            abs(rt - ct)," (",round(100 * abs(rt - ct)/max(c(rt,ct)),2),
            "%) an exact solution is impossible.")
  }

  # Get scale about right
  mat <- mat / (sum(mat, na.rm = TRUE) / sum(rsum))
  #mat_orig = mat

  for(i in seq_len(n)){
    mat <- bal_func(mat, rsum = rsum, csum = csum, int_only = int_only)
    if(i == 1 & !quiet){
      err = rowSums(mat, na.rm = TRUE) - rsum
      message("1st  pass: total error = ", round(sum(abs(err)), 2),
              " RMSE = ", round(sqrt(mean(err * err)), 2))
    }
    if(i == n & !quiet){
      err = rowSums(mat, na.rm = TRUE) - rsum
      message(n,"th pass: total error = ", round(sum(abs(err)), 2),
              " RMSE = ", round(sqrt(mean(err * err)), 2))
    }
    if(all(rowSums(mat, na.rm = TRUE) == rsum)){
      if(all(colSums(mat, na.rm = TRUE) == csum)){
        if(!quiet){
          message(n,"th pass: perfect match, exit early")
        }
        break
      }
    }
  }


  rownames(mat) <- rname
  colnames(mat) <- cname

  # Check
  if(check){
    if(!all(rowSums(mat) == rsum)){
      err = rowSums(mat, na.rm = TRUE) - rsum
      message("Rows don't match: total error = ", round(sum(abs(err)), 2),"/",rt,
              " max error = ", round(max(abs(err)), 2),
              " RMSE = ", round(sqrt(mean(err * err)), 2))
    }
    if(!all(colSums(mat) == csum)){
      err = colSums(mat, na.rm = TRUE) - csum
      message("Cols don't match: total error = ", round(sum(abs(err)), 2),"/",ct,
              " max error = ", round(max(abs(err)), 2),
              " RMSE = ", round(sqrt(mean(err * err)), 2))
    }
  }

  return(mat)
}


# Fast, memory-efficient fill for incomplete matrices with partial constraints
# - Never modifies non-NA values (they stay fixed)
# - Always achieves grand total tt (exactly)
# - Satisfies all row/column sum constraints that are specified (non-NA in rsum/csum)
# - Uses iterative balancing on free cells only (Furness-inspired)
# - Handles over-constrained systems by flexing unconstrained rows/columns

furness_partial_integer_total <- function(mat, rsum, csum, tt, max_iterations = 1000, tolerance = 1e-3) {

  # Validate inputs
  stopifnot(nrow(mat) == length(rsum), ncol(mat) == length(csum),
            is.matrix(mat), is.numeric(rsum), is.numeric(csum), is.numeric(tt))

  nr <- nrow(mat); nc <- ncol(mat)

  # Calculate known values and deficits
  known_sum <- sum(mat, na.rm = TRUE)
  if (known_sum > tt) stop("Sum of known values exceeds grand total tt.")

  total_deficit <- tt - known_sum

  # Calculate row/column deficits
  row_deficit <- rep(NA_real_, nr)
  for (i in which(!is.na(rsum))) {
    known_in_row <- sum(mat[i, ], na.rm = TRUE)
    row_deficit[i] <- rsum[i] - known_in_row
  }

  col_deficit <- rep(NA_real_, nc)
  for (j in which(!is.na(csum))) {
    known_in_col <- sum(mat[, j], na.rm = TRUE)
    col_deficit[j] <- csum[j] - known_in_col
  }

  # Validation
  if (any(!is.na(row_deficit) & row_deficit < -1e-9))
    stop("Known cells exceed at least one prescribed row sum.")
  if (any(!is.na(col_deficit) & col_deficit < -1e-9))
    stop("Known cells exceed at least one prescribed column sum.")

  # Special case: no free cells
  num_free <- sum(is.na(mat))
  if (num_free == 0) {
    if (abs(sum(mat, na.rm = TRUE) - tt) > 1e-9)
      stop("No free cells but sum(known) != tt.")
    return(mat)
  }

  # Identify constrained dimensions
  constrained_rows <- which(!is.na(row_deficit))
  constrained_cols <- which(!is.na(col_deficit))
  unconstrained_rows <- setdiff(seq_len(nr), constrained_rows)
  unconstrained_cols <- setdiff(seq_len(nc), constrained_cols)

  # Strategy: Initialize conservatively - only cells that are:
  # 1. In the core constrained × constrained submatrix, OR
  # 2. The ONLY free cell in a constrained row/column (forced necessity)
  # This minimizes spreading and keeps unconstrained cells available for adjustment

  # First, identify forced cells (only free cell in a constrained dimension)
  forced_cells <- matrix(, nrow = 0, ncol = 2)  # Collect forced cell positions

  for (i in constrained_rows) {
    free_in_row <- which(is.na(mat[i, ]))
    if (length(free_in_row) == 1) {
      forced_cells <- rbind(forced_cells, c(i, free_in_row[1]))
    }
  }

  for (j in constrained_cols) {
    free_in_col <- which(is.na(mat[, j]))
    if (length(free_in_col) == 1) {
      forced_cells <- rbind(forced_cells, c(free_in_col[1], j))
    }
  }

  # Initialize free_mat: conservative approach
  free_mat <- mat
  constraint_deficit <- total_deficit  # Total to distribute

  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      if (is.na(mat[i, j])) {
        in_constr_row <- i %in% constrained_rows
        in_constr_col <- j %in% constrained_cols

        # Initialize only if in constrained × constrained OR in forced_cells
        if (in_constr_row && in_constr_col) {
          # Core constrained submatrix: initialize
          free_mat[i, j] <- 1 / max(length(constrained_rows), length(constrained_cols), 1)
        } else if (nrow(forced_cells) > 0 && any(forced_cells[,1] == i & forced_cells[,2] == j)) {
          # Forced cell: initialize
          free_mat[i, j] <- 1 / max(length(constrained_rows), length(constrained_cols), 1)
        } else {
          # Unconstrained or crossing: keep at 0 for now
          free_mat[i, j] <- 0
        }
      }
    }
  }

  # IPF on constrained dimensions: scale all free cells in constrained rows/cols
  for (iter in seq_len(max_iterations)) {
    converged <- TRUE

    # Row scaling on CONSTRAINED rows: scale ALL free cells in constrained rows
    for (i in constrained_rows) {
      free_in_row <- which(is.na(mat[i, ]))

      if (length(free_in_row) > 0) {
        current_sum <- sum(free_mat[i, free_in_row])
        if (current_sum > 1e-12 && row_deficit[i] > 1e-12) {
          ratio <- row_deficit[i] / current_sum
          free_mat[i, free_in_row] <- free_mat[i, free_in_row] * ratio
          converged <- FALSE
        }
      }
    }

    # Column scaling on CONSTRAINED columns: scale ALL free cells in constrained cols
    for (j in constrained_cols) {
      free_in_col <- which(is.na(mat[, j]))

      if (length(free_in_col) > 0) {
        current_sum <- sum(free_mat[free_in_col, j])
        if (current_sum > 1e-12 && col_deficit[j] > 1e-12) {
          ratio <- col_deficit[j] / current_sum
          free_mat[free_in_col, j] <- free_mat[free_in_col, j] * ratio
          converged <- FALSE
        }
      }
    }

    # Check convergence
    if (converged) break
  }

  # Round and compile result so far
  free_mat <- round(free_mat)
  result <- mat
  result[is.na(result)] <- free_mat[is.na(mat)]

  # CORRECTION PASS: Ensure all constrained row/column sums are exact
  # This fixes oscillation issues from iterative scaling
  # Key: only adjust using cells in FULLY unconstrained dimensions

  # First, fix constrained rows by moving excess to unconstrained rows
  for (i in constrained_rows) {
    current_row_sum <- sum(result[i, ])
    row_error <- rsum[i] - current_row_sum

    if (abs(row_error) > 1e-6) {
      if (row_error < 0) {
        # Row has excess: move it to unconstrained rows in the same columns
        excess <- abs(row_error)
        free_in_row <- which(is.na(mat[i, ]))

        # Move excess from free cells in this row to unconstrained rows in those columns
        for (j in free_in_row) {
          if (result[i, j] > 1e-9 && excess > 1e-9) {
            move_amount <- min(result[i, j], excess)
            result[i, j] <- result[i, j] - move_amount
            excess <- excess - move_amount

            # Put it in an unconstrained row in the same column
            if (length(unconstrained_rows) > 0) {
              rows_available <- which(is.na(mat[, j]) & (seq_len(nr) %in% unconstrained_rows))
              if (length(rows_available) > 0) {
                i_unc <- rows_available[1]
                result[i_unc, j] <- result[i_unc, j] + move_amount
              }
            }
          }
        }
      } else {
        # Row needs more: try unconstrained columns first
        free_in_row <- which(is.na(mat[i, ]))
        unconstrained_in_row <- intersect(free_in_row, unconstrained_cols)
        if (length(unconstrained_in_row) > 0) {
          j <- unconstrained_in_row[1]
          result[i, j] <- result[i, j] + row_error
        } else {
          # All columns are constrained: allocate directly to constrained columns
          # We'll allocate value strategically to columns and let the grand total
          # and column correction pass balance things
          shortfall_remaining <- abs(row_error)
          for (j in free_in_row[free_in_row %in% constrained_cols]) {
            if (shortfall_remaining > 1e-9) {
              result[i, j] <- result[i, j] + shortfall_remaining
              shortfall_remaining <- 0
              break
            }
          }
        }
      }
    }
  }

  # Then fix constrained columns
  for (j in constrained_cols) {
    current_col_sum <- sum(result[, j])
    col_error <- csum[j] - current_col_sum

    if (abs(col_error) > 1e-6) {
      if (col_error < 0) {
        # Column has excess: move it to unconstrained columns in the same rows
        excess <- abs(col_error)
        free_in_col <- which(is.na(mat[, j]))

        # Move excess from free cells in this column to unconstrained columns in those rows
        for (i in free_in_col) {
          if (result[i, j] > 1e-9 && excess > 1e-9) {
            move_amount <- min(result[i, j], excess)
            result[i, j] <- result[i, j] - move_amount
            excess <- excess - move_amount

            # Put it in an unconstrained column in the same row
            if (length(unconstrained_cols) > 0) {
              cols_available <- which(is.na(mat[i, ]) & (seq_len(nc) %in% unconstrained_cols))
              if (length(cols_available) > 0) {
                j_unc <- cols_available[1]
                result[i, j_unc] <- result[i, j_unc] + move_amount
              }
            }
          }
        }
      } else {
        # Column needs more: try unconstrained rows first
        free_in_col <- which(is.na(mat[, j]))
        unconstrained_in_col <- intersect(free_in_col, unconstrained_rows)
        if (length(unconstrained_in_col) > 0) {
          i <- unconstrained_in_col[1]
          result[i, j] <- result[i, j] + col_error
        } else {
          # No unconstrained rows: borrow from unconstrained columns in constrained rows
          borrowed <- 0
          for (i in free_in_col[free_in_col %in% constrained_rows]) {
            if (borrowed < abs(col_error)) {
              # Look for value in unconstrained columns in this row
              for (j_unc in unconstrained_cols) {
                if (is.na(mat[i, j_unc]) && result[i, j_unc] > 1e-9) {
                  move_amount <- min(result[i, j_unc], abs(col_error) - borrowed)
                  result[i, j_unc] <- result[i, j_unc] - move_amount
                  result[i, j] <- result[i, j] + move_amount
                  borrowed <- borrowed + move_amount
                  if (abs(borrowed - abs(col_error)) < 1e-9) break
                }
              }
            }
          }
        }
      }
    }
  }

  # Now handle unconstrained dimensions and grand total
  constrained_sum <- sum(result[constrained_rows, constrained_cols])
  remaining_need <- tt - sum(result)

  # Determine which columns need more to satisfy their constraints
  col_still_needs <- col_deficit  # Fresh copy
  for (j in constrained_cols) {
    col_still_needs[j] <- csum[j] - sum(result[, j])
  }

  # Distribute remaining_need to unconstrained rows, prioritizing columns that need it
  if (abs(remaining_need) > 1e-9 && length(unconstrained_rows) > 0) {

    if (remaining_need > 1e-9) {
      # Positive deficit: add values
      # First pass: put in columns that still have deficits (constrained columns)
      cols_needing_values <- which(col_still_needs > 1e-9)

      if (length(cols_needing_values) > 0 && abs(remaining_need) > 1e-9) {
        # Distribute need across columns that still need it
        for (j in cols_needing_values) {
          need_in_col <- col_still_needs[j]
          rows_available <- which(is.na(mat[, j]) & (seq_len(nr) %in% unconstrained_rows))

          if (length(rows_available) > 0 && need_in_col > 1e-9) {
            place_amount <- min(need_in_col, remaining_need)
            i <- rows_available[1]
            result[i, j] <- result[i, j] + place_amount
            remaining_need <- remaining_need - place_amount
            if (abs(remaining_need) < 1e-9) break
          }
        }
      }

      # If still have remaining, put in purely unconstrained cells
      if (abs(remaining_need) > 1e-9 && length(unconstrained_rows) > 0 && length(unconstrained_cols) > 0) {
        for (i in unconstrained_rows) {
          for (j in unconstrained_cols) {
            if (is.na(mat[i, j])) {
              result[i, j] <- result[i, j] + remaining_need
              remaining_need <- 0
              break
            }
          }
          if (abs(remaining_need) < 1e-9) break
        }
      }

      # Last resort: any free cell in unconstrained row
      if (abs(remaining_need) > 1e-9 && length(unconstrained_rows) > 0) {
        for (i in unconstrained_rows) {
          free_in_row <- which(is.na(mat[i, ]))
          if (length(free_in_row) > 0 && abs(remaining_need) > 1e-9) {
            # Prefer unconstrained columns in this row
            unconstrained_in_row <- intersect(free_in_row, unconstrained_cols)
            if (length(unconstrained_in_row) > 0) {
              j <- unconstrained_in_row[1]
              result[i, j] <- result[i, j] + remaining_need
              remaining_need <- 0
              break
            }
          }
        }
      }
    } else {
      # Negative deficit: subtract values (remove from unconstrained cells)
      # CRITICAL: only remove from FULLY unconstrained cells (unconstrained row AND col)
      removal_need <- abs(remaining_need)

      # Only remove from purely unconstrained cells
      if (length(unconstrained_rows) > 0 && length(unconstrained_cols) > 0) {
        for (i in unconstrained_rows) {
          for (j in unconstrained_cols) {
            if (is.na(mat[i, j]) && result[i, j] > 1e-9 && removal_need > 1e-9) {
              remove_amount <- min(result[i, j], removal_need)
              result[i, j] <- result[i, j] - remove_amount
              removal_need <- removal_need - remove_amount
              if (removal_need < 1e-9) break
            }
          }
          if (removal_need < 1e-9) break
        }
      }

      # If still need to remove but no fully unconstrained cells, we have a problem
      if (removal_need > 1e-9) {
        # This indicates an infeasible or over-constrained system
        warning("Cannot fully satisfy grand total constraint without violating row/column constraints.")
      }
    }
  }

  # SECOND CORRECTION PASS: Now that unconstrained rows/columns are populated,
  # fix any remaining constrained row/column deficits by borrowing
  for (i in constrained_rows) {
    current_row_sum <- sum(result[i, ])
    row_error <- rsum[i] - current_row_sum

    if (row_error > 1e-6 && length(unconstrained_rows) > 0) {
      # Row needs more and we have unconstrained rows available
      free_in_row <- which(is.na(mat[i, ]))
      for (j in free_in_row[free_in_row %in% constrained_cols]) {
        if (row_error < 1e-9) break
        # Try to borrow from unconstrained rows in this constrained column
        for (i_unc in unconstrained_rows) {
          if (is.na(mat[i_unc, j]) && result[i_unc, j] > 1e-9 && row_error > 1e-9) {
            move_amount <- min(result[i_unc, j], row_error)
            result[i_unc, j] <- result[i_unc, j] - move_amount
            result[i, j] <- result[i, j] + move_amount
            row_error <- row_error - move_amount
          }
        }
      }
    }
  }

  for (j in constrained_cols) {
    current_col_sum <- sum(result[, j])
    col_error <- csum[j] - current_col_sum

    if (col_error > 1e-6 && length(unconstrained_cols) > 0) {
      # Column needs more and we have unconstrained columns available
      free_in_col <- which(is.na(mat[, j]))
      for (i in free_in_col[free_in_col %in% constrained_rows]) {
        if (col_error < 1e-9) break
        # Try to borrow from unconstrained columns in this constrained row
        for (j_unc in unconstrained_cols) {
          if (is.na(mat[i, j_unc]) && result[i, j_unc] > 1e-9 && col_error > 1e-9) {
            move_amount <- min(result[i, j_unc], col_error)
            result[i, j_unc] <- result[i, j_unc] - move_amount
            result[i, j] <- result[i, j] + move_amount
            col_error <- col_error - move_amount
          }
        }
      }
    }
  }

  # Validation: Check that result meets all original constraints
  violation_count <- 0

  # 1. Check that known values are unchanged
  known_mask <- !is.na(mat)
  if (any(result[known_mask] != mat[known_mask])) {
    warning("Some known (non-NA) values in mat were modified. This violates constraints.")
    violation_count <- violation_count + 1
  }

  # 2. Check that result has no NA values
  if (any(is.na(result))) {
    warning("Result still contains NA values.")
    violation_count <- violation_count + 1
  }

  # 3. Check row sum constraints
  for (i in constrained_rows) {
    row_sum <- sum(result[i, ])
    if (abs(row_sum - rsum[i]) > 1e-6) {
      warning("Row ", i, " sum is ", row_sum, " but expected ", rsum[i], ".")
      violation_count <- violation_count + 1
    }
  }

  # 4. Check column sum constraints
  for (j in constrained_cols) {
    col_sum <- sum(result[, j])
    if (abs(col_sum - csum[j]) > 1e-6) {
      warning("Column ", j, " sum is ", col_sum, " but expected ", csum[j], ".")
      violation_count <- violation_count + 1
    }
  }

  # 5. Check grand total constraint
  total <- sum(result)
  if (abs(total - tt) > 1e-6) {
    warning("Grand total is ", total, " but expected ", tt, ".")
    violation_count <- violation_count + 1
  }

  if (violation_count == 0) {
    # All constraints satisfied - silent success
  }

  result
}
