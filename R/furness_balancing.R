bal_func <- function(mat2, rsum2, csum2, int_only = FALSE){
  # Find ratio of rows
  mat_rsum <- rowSums(mat2, na.rm = TRUE)
  mat_rratio <- rsum2 / mat_rsum
  mat_rratio[is.nan(mat_rratio)] <- 0

  mat2 <- mat2 * mat_rratio

  if(int_only){
    mat2 <- round_half_random(mat2)
  }

  # Find ratio of rows
  mat_csum <- colSums(mat2, na.rm = TRUE)
  mat_cratio <- csum2 / mat_csum
  mat_cratio[is.nan(mat_cratio)] <- 0

  mat2 <- sweep(mat2, MARGIN=2, mat_cratio, `*`)
  mat2[is.nan(mat2)] <- 0

  if(int_only){
    mat2 <- round_half_random(mat2)
  }

  return(mat2)
}

round_half_random <- function(x) {
  tweaks <- runif(length(x), min = -0.5, max = 0.5)
  # Never tweak to 0
  tweaks <- ifelse(x < 1, 0, tweaks)
  #x <- ifelse(x %% 0.5 == 0, x + tweaks, x)
  round(x + tweaks)
}


# Furness method balancing
furness_partial <- function(mat, rsum, csum, n = 100, check = TRUE){

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
                           int_only = TRUE)
    # Are we stuck in a loop?
    if(identical(mat_old, mat_change)){
      mat_change <- bal_func(mat2 = mat_change_orig,
                             rsum2 = rsum_change,
                             csum2 = csum_change,
                             int_only = TRUE)
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
      print("\n")
      print(mat)
      print(rsum)
      print(csum)
      stop("Rows don't match ",i)
    }
    if(!all(colSums(mat_fin) == csum)){
      print("\n")
      print(mat)
      print(rsum)
      print(csum)
      stop("Cols don't match ",i)
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
