# Fast grouped IPF engine ----------------------------------------------------
#
# NEW CODE (2026-07): shared engine for the _v2 synthetic population functions
# in synthetic_pop_cenus3_v2.R and synthetic_pop_scotland_v2.R. The originals
# (sythetic_census / sythetic_census_scot) are unchanged, so old and new
# methods can be cross-compared. See scripts/compare_v1_v2.R.
#
# Why this exists: mipfp::Ipfp only accepts constraints that are full
# cross-product margins of the seed array. To use coarse-category census
# tables (e.g. CarVan3, Tenure3, AccType3, householdComp4) the v1 code
# therefore carried the coarse classifications as EXTRA array dimensions,
# inflating the Scottish seed from 8,750 cells (fine categories only) to
# 945,000 cells - a 108x blow-up swept on every iteration by R-level apply().
#
# This engine implements "grouped" IPF instead: each constraint is described
# by an integer group index that maps every cell of the (fine-dimension-only)
# array to a cell of the constraint table. A margin update is then one
# rowsum() call (C level) plus one vectorised multiply. Coarse categories are
# handled by mapping fine levels to coarse levels in the group index, so the
# redundant dimensions disappear entirely.
#
# The update rule, stopping rule, inconsistent-margin handling and
# error.margins reporting deliberately replicate mipfp::Ipfp so results are
# directly comparable:
#  - if constraint totals disagree (> tol.margins) the seed and all targets
#    are normalised to probabilities (this is virtually always the case for
#    zone-level census tables due to disclosure control), so x.hat is on the
#    probability scale and callers multiply by med_pop exactly as v1 does;
#  - update factor is 0 where the target or the current margin is 0;
#  - convergence when max(abs(x - x_prev)) < tol.


#' Build a constraint group index for grouped IPF
#'
#' @description Converts per-cell category indices into a single integer
#'   index identifying, for every cell of the seed array, the cell of the
#'   constraint table it contributes to. Constraint tables are assumed to
#'   be stored column-major (first dimension fastest), i.e. exactly as
#'   produced by `array_maker()` / `as.numeric()` on the constraint array.
#' @param idx_cols List of integer vectors (one per constraint dimension, in
#'   constraint-array dimension order), each of length `prod(dim(seed))`,
#'   giving the (possibly coarse-mapped) category index of every seed cell.
#' @param sizes Integer vector of constraint-array dimension sizes, same
#'   order as `idx_cols`.
#' @return A list with `g` (integer group index per seed cell), `K` (number
#'   of constraint cells) and `present` (sorted unique groups, precomputed
#'   for the rowsum alignment in `ipf_grouped()`).
#' @keywords internal
ipf_group_index = function(idx_cols, sizes){
  g = idx_cols[[1]]
  mult = sizes[1]
  if(length(idx_cols) > 1){
    for(j in 2:length(idx_cols)){
      g = g + (idx_cols[[j]] - 1L) * mult
      mult = mult * sizes[j]
    }
  }
  g = as.integer(g)
  list(g = g, K = as.integer(prod(sizes)), present = sort(unique(g)))
}


#' Iterative proportional fitting with grouped (coarse-category) margins
#'
#' @description Drop-in replacement for the `mipfp::Ipfp` calls in the
#'   synthetic population build, operating on the fine-category seed only.
#'   Accepts any constraint expressible as a grouping of seed cells,
#'   including coarse-category census tables, removing the need for
#'   redundant array dimensions. Matches mipfp behaviour: normalises seed
#'   and targets to probabilities when target totals are inconsistent,
#'   zeroes cells whose target or current margin is zero, and stops when
#'   the largest cell change falls below `tol`.
#' @param seed Numeric vector (or array) of seed values, fine categories
#'   only, column-major.
#' @param target.data List of numeric vectors/arrays, the constraint tables
#'   (same cell order as the group indices assume).
#' @param group.list List of group-index specs from `ipf_group_index()`,
#'   same length and order as `target.data`.
#' @param iter Maximum number of iterations.
#' @param tol Stopping tolerance on max absolute cell change. v1 used the
#'   mipfp default 1e-10, which zone-level census tables (perturbed by
#'   disclosure control) essentially never reach; 1e-6 stops once the fit
#'   has stabilised instead of burning the full iteration budget.
#' @param tol.margins Tolerance used to decide whether the constraint
#'   totals are consistent (as in mipfp).
#' @return A list with `x.hat` (fitted vector, probability scale when
#'   targets were inconsistent), `conv` (logical), `error.margins`
#'   (per-constraint max abs deviation, same definition as mipfp),
#'   `stp.crit` (final stopping criterion) and `iterations` (number used).
#' @keywords internal
ipf_grouped = function(seed, target.data, group.list,
                       iter = 1000, tol = 1e-6, tol.margins = 1e-10){

  x = as.numeric(seed)
  targets = lapply(target.data, as.numeric)
  nT = length(targets)

  if(nT != length(group.list)){stop("target.data and group.list lengths differ")}
  if(min(vapply(targets, min, numeric(1))) < 0 || min(x) < 0){
    stop("Target and seed cells must be non-negative")
  }

  # As mipfp: shift to probabilities when the constraint totals disagree
  if(nT > 1){
    tsums = vapply(targets, sum, numeric(1))
    if(any(abs(diff(tsums)) > tol.margins)){
      x = x / sum(x)
      targets = lapply(targets, function(t){t / sum(t)})
    }
  }

  conv = FALSE
  crit = NA_real_
  i = 0L
  for(i in seq_len(iter)){
    x_prev = x
    for(j in seq_len(nT)){
      gl = group.list[[j]]
      m = rowsum(x, gl$g)[, 1] # sums ordered by sort(unique(g)) = gl$present
      t_here = targets[[j]][gl$present]
      f = t_here / m
      f[!is.finite(f) | t_here == 0] = 0 # target==0 or margin==0 -> 0, as mipfp
      f_full = numeric(gl$K)
      f_full[gl$present] = f
      x = x * f_full[gl$g]
    }
    crit = max(abs(x - x_prev))
    if(crit < tol){
      conv = TRUE
      break
    }
  }

  err = numeric(nT)
  for(j in seq_len(nT)){
    gl = group.list[[j]]
    m_full = numeric(gl$K)
    m_full[gl$present] = rowsum(x, gl$g)[, 1]
    err[j] = max(abs(targets[[j]] - m_full))
  }
  names(err) = names(target.data)

  list(x.hat = x, conv = conv, error.margins = err,
       stp.crit = crit, iterations = i)
}


#' Derive a fine-to-coarse category mapping from a national seed margin
#'
#' @description The national seed arrays encode which coarse category each
#'   fine category belongs to: cells combining a fine level with the wrong
#'   coarse level carry only the 1e-15 floor added by the seed builders.
#'   This function recovers the mapping from the fine x coarse margin of
#'   the seed and errors if any fine level does not map overwhelmingly to a
#'   single coarse level, rather than silently guessing.
#' @param joint Numeric matrix, fine levels in rows, coarse levels in
#'   columns (a margin of the national seed array).
#' @param what Label used in error messages.
#' @param dominance Maximum tolerated share of a fine level's mass outside
#'   its main coarse level.
#' @return Named integer vector: for each fine level (row), the index of
#'   its coarse level (column).
#' @keywords internal
derive_grouping = function(joint, what = "mapping", dominance = 1e-6){
  map = integer(nrow(joint))
  for(i in seq_len(nrow(joint))){
    row = joint[i, ]
    tot = sum(row)
    top = which.max(row)
    if(tot <= 0 || (tot - row[top]) > dominance * tot){
      stop("Ambiguous fine-to-coarse ", what, " for level '",
           rownames(joint)[i], "': seed mass ",
           paste(signif(row, 3), collapse = ", "),
           ". Supply the mapping explicitly.")
    }
    map[i] = top
  }
  names(map) = rownames(joint)
  map
}


#' Mean absolute error of an integerised table against one constraint
#'
#' @description Grouped-margin replacement for `vaidate_syth_pop()`: sums
#'   the integerised fine-category counts into the constraint's cells via
#'   the precomputed group index and returns
#'   `sum(abs(achieved - target)) / sum(target != 0)`, the same metric as
#'   v1 but computed by position (immune to the row/column name-ordering
#'   assumptions of the pivot_wider approach) and hundreds of times faster.
#' @param x_int Integerised fine-category counts (vector or array,
#'   column-major).
#' @param spec Group-index spec from `ipf_group_index()`.
#' @param target Constraint table (array or vector, counts).
#' @return Mean absolute error (single numeric).
#' @keywords internal
grouped_mae = function(x_int, spec, target){
  t_vec = as.numeric(target)
  m_full = numeric(spec$K)
  m_full[spec$present] = rowsum(as.numeric(x_int), spec$g)[, 1]
  sum(abs(m_full - t_vec)) / sum(t_vec != 0)
}
