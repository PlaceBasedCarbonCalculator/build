#' Convert numeric values to A+ to F- grades by percentile
#'
#' @description Ranks values into percentiles (via `percentile()` in
#'   flights.R) and maps each percentile to an 18-band letter grade from "A+"
#'   (best) to "F-" (worst), with wider bands in the middle of the
#'   distribution: 1% at each extreme, 7-8% through the middle. Used for the
#'   emissions grades shown on the Carbon & Place map (see
#'   `combine_lsoa_emissions()` and `select_map_outputs()`).
#' @param x Numeric vector to grade.
#' @param high_good Logical. If TRUE, high values get good grades (e.g. a
#'   large percentage emissions reduction is an "A+"); if FALSE (default), low
#'   values get good grades (e.g. low emissions).
#' @param zeroNA Logical, passed to `percentile()`. If TRUE, zeros are treated
#'   as missing when computing percentile breaks.
#' @return A character vector of grades; missing values become the string "NA".
#' @keywords internal
value2grade <- function(x, high_good = FALSE, zeroNA = TRUE){

  x_cent <- percentile(x, zeroNA = zeroNA) # In flights.R

  # One entry per percentile band, which `percentile()` numbers 0-99, so these
  # must sum to 100. They used to be padded to 101 - "A+" prepended when low
  # values are good, "F-" appended when high values are good - to cover a 101st
  # band that only ever held the single most extreme zone. The padding made F-
  # two full percent wide on the high-is-good layer while every other band
  # stayed at one; `percentile()` now returns 100 genuine bands, so no padding
  # is needed and both extremes are 1% in either direction.
  grades <- c(rep("A+",1),
              rep("A",4),
              rep("A-",5),
              rep("B+",6),
              rep("B",6),
              rep("B-",7),
              rep("C+",7),
              rep("C",7),
              rep("C-",7),
              rep("D+",7),
              rep("D",7),
              rep("D-",7),
              rep("E+",7),
              rep("E",6),
              rep("E-",6),
              rep("F+",5),
              rep("F",4),
              rep("F-",1))
  stopifnot(length(grades) == 100)

  if(high_good){
    x_grade <- grades[match(x_cent,99:0)]
  } else {
    x_grade <- grades[match(x_cent,0:99)]
  }

  x_grade[is.na(x_grade)] = "NA"

  return(x_grade)
}
