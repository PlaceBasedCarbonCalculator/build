#!/usr/bin/env Rscript
# Debug the furness function

source("R/furness_balancing.R")

# Based on the user's description: row 1 needs 5, rows/cols are partially constrained
# Let me try to construct a test case matching their output [1,1]=0, [1,2]=4, [2,1]=137, [2,2]=1, [3,1]=1, [3,2]=4, [4,1]=3, [4,2]=7

# If these are the row sums: 0+4=4, 137+1=138, 1+4=5, 3+7=10
# Column sums: 0+137+1+3=141, 4+1+4+7=16
# Grand total: 157

# Working backwards:
# Row 1 needs some constraint that would require 5, currently 4 -> deficit of 1
# Col 1 sum is 141, Col 2 sum is 16, total 157

# Let me try: rsum = c(5, 138, NA, NA), csum = c(141, 16), tt = 157
# And start mat with some known values

mat_test <- matrix(c(
  NA, NA,      # Row 1: needs 5 total
  NA, 1,       # Row 2: needs 138 total, has 1 in col 2
  NA, NA,      # Rows 3+: unconstrained
  NA, NA
), nrow = 4, ncol = 2, byrow = TRUE)

rsum_test <- c(5, 138, NA, NA)
csum_test <- c(141, 16)
tt_test <- 157

cat("Test input:\n")
print(mat_test)
cat("\nrsum:", rsum_test, "\n")
cat("csum:", csum_test, "\n")
cat("tt:", tt_test, "\n\n")

result <- tryCatch({
  furness_partial_integer_total(mat_test, rsum_test, csum_test, tt_test)
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
  return(NULL)
})

if (!is.null(result)) {
  cat("Result:\n")
  print(result)
  cat("\nRow sums:", rowSums(result), "\n")
  cat("Col sums:", colSums(result), "\n")
  cat("Total:", sum(result), "\n")
  cat("\nRow 1 sum is 5?", rowSums(result)[1] == 5, "\n")
  cat("Col 1 sum is 141?", colSums(result)[1] == 141, "\n")
} else {
  cat("Failed\n")
}
