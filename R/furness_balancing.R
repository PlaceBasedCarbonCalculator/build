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


# Integer fill for partial margins with a grand total
# - Enforces provided rows/cols and the grand total exactly (integers)
# - Keeps known cells fixed
# - Optionally nudges toward a seed (e.g., IPF result)

furness_partial_integer_total <- function(mat, rsum, csum, tt, seed = NULL) {
  if (!requireNamespace("lpSolve", quietly = TRUE)) {
    stop("Package 'lpSolve' is required. Install with install.packages('lpSolve').")
  }
  nr <- nrow(mat); nc <- ncol(mat)
  free <- which(is.na(mat), arr.ind = TRUE)
  k <- nrow(free)

  K <- replace(mat, is.na(mat), 0)
  sum_K <- sum(K)
  T_res <- tt - sum_K
  if (T_res < -1e-12) stop("Known cells exceed 'tt'.")

  R <- rsum
  C <- csum
  R_res <- ifelse(!is.na(R), R - rowSums(K), NA_real_)
  C_res <- ifelse(!is.na(C), C - colSums(K), NA_real_)
  if (any(!is.na(R_res) & R_res < -1e-12)) stop("Known cells exceed provided row totals.")
  if (any(!is.na(C_res) & C_res < -1e-12)) stop("Known cells exceed provided column totals.")

  # Build constraints: provided rows, provided cols, and total
  n_constr <- sum(!is.na(R_res)) + sum(!is.na(C_res)) + 1
  A <- matrix(0, nrow = n_constr, ncol = max(k, 1))
  rhs <- numeric(n_constr)
  dir <- rep("=", n_constr)

  rmap <- which(!is.na(R_res))
  cmap <- which(!is.na(C_res))

  ptr <- 0
  # Rows
  for (i in rmap) {
    ptr <- ptr + 1
    if (k > 0) {
      idx <- which(free[,1] == i)
      if (length(idx)) A[ptr, idx] <- 1
    }
    rhs[ptr] <- R_res[i]
  }
  # Columns
  for (j in cmap) {
    ptr <- ptr + 1
    if (k > 0) {
      idx <- which(free[,2] == j)
      if (length(idx)) A[ptr, idx] <- 1
    }
    rhs[ptr] <- C_res[j]
  }
  # Total
  ptr <- ptr + 1
  if (k > 0) A[ptr, seq_len(k)] <- 1
  rhs[ptr] <- T_res

  if (k == 0) {
    # Nothing to optimize: just check constraints
    if (abs(sum_K - tt) > 1e-9) stop("No NA cells but sum(known) != tt.")
    if (length(rmap)) {
      if (max(abs(rowSums(K)[rmap] - rsum[rmap])) > 1e-9)
        stop("Row totals not met by known cells.")
    }
    if (length(cmap)) {
      if (max(abs(colSums(K)[cmap] - csum[cmap])) > 1e-9)
        stop("Column totals not met by known cells.")
    }
    return(mat)
  }

  # Objective: tie-break using a seed if provided (encourage closeness)
  if (is.null(seed)) {
    obj <- rep(1, k)
  } else {
    if (!all(dim(seed) == dim(mat))) stop("'seed' must match 'mat' dimensions.")
    s <- pmax(0, seed[is.na(mat)])
    # Simple monotone weights toward the seed magnitudes (heuristic)
    obj <- pmax(1, round(1000 / (1 + s)))
  }

  sol <- lpSolve::lp(
    direction   = "min",
    objective.in = obj,
    const.mat    = A,
    const.dir    = dir,
    const.rhs    = round(rhs),   # integer RHS
    all.int      = TRUE
  )
  if (sol$status != 0) {
    stop("Integer model infeasible; check 'tt' and provided margins vs known cells.")
  }

  out <- mat
  out[is.na(out)] <- pmax(0, round(sol$solution))
  out
}
