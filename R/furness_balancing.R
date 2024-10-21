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
  for(i in seq(1, length(combinations))){
    mat_sub <- mat
    mat_sub[na_indices] <- combinations[[i]]

    rsum_mat <- rowSums(mat_sub)
    csum_mat <- colSums(mat_sub)

    if(!all(rsum_mat == rsum, na.rm = TRUE)){
      mat_sub <- NULL
    }
    if(!all(csum_mat == csum, na.rm = TRUE)){
      mat_sub <- NULL
    }
    combinations_mat[[i]] <- mat_sub
  }

  combinations_mat <- combinations_mat[lengths(combinations_mat) > 0]
  mat_fin <- combinations_mat[[sample(seq_along(combinations_mat), 1)]]

  return(mat_fin)
}


generate_combinations <- function(t, n, prefix = numeric()) {
  result <- list()
  if (n == 1) {
    if (t > 0) {
      result <- list(c(prefix, t))
    }
  } else {
    if (n < 1 || t <= 0) return(list())
    for (i in t:1) {
      temp <- generate_combinations(t - i, n - 1, c(prefix, i))
      result <- c(result, temp)
    }
  }
  return(result)
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
