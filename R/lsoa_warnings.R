#' Build data-quality warning codes for each zone
#'
#' @description Flags zones whose historical data should be interpreted with
#'   caution, for display on the website. Warning codes: 1 = Scottish Data
#'   Zone (2011 data estimated onto 2022 zones); 2/3/4 = 2021 LSOA was
#'   fragmented (X) / merged (M) / split (S) relative to 2011; 5 = zone has
#'   zero population in some year; 6 = zone has an implausible company/bike
#'   emissions rate in some year, derived from the `company_bike_suppressed`
#'   flag set by `combine_lsoa_emissions()` so the warning shown always
#'   matches the data actually suppressed. Bespoke warnings are appended from
#'   `data/lsoa_warnings.csv` (which still carries the hand-curated code 6 and
#'   code 7 zones; duplicates are dropped). Used by the `lsoa_warnings`
#'   target, which feeds `make_lsoa_overview_json()`.
#' @param lookup_lsoa_2011_21 ONS 2011-to-2021 LSOA lookup with `CHGIND`.
#' @param lookup_dz_2011_22 Simplified Data Zone 2011-to-2022 lookup (all its
#'   zones get warning code 1).
#' @param population GB population table with `LSOA21CD` and `all_ages`.
#' @param lsoa_emissions_all Master emissions table (`lsoa_emissions_all`),
#'   read only for its `company_bike_suppressed` column.
#' @return A data frame with `LSOA21CD` and `warningcode`, one row per
#'   zone/warning, sorted by zone code.
#' @keywords internal
make_lsoa_warnings = function(lookup_lsoa_2011_21,lookup_dz_2011_22, population,
                              lsoa_emissions_all){

  lookup_dz_2011_22 = lookup_dz_2011_22[,"LSOA21CD"]
  lookup_dz_2011_22 = lookup_dz_2011_22[!duplicated(lookup_dz_2011_22$LSOA21CD),]

  lookup_lsoa_2011_21 = lookup_lsoa_2011_21[lookup_lsoa_2011_21$CHGIND != "U",]
  lookup_lsoa_2011_21 = lookup_lsoa_2011_21[,c("LSOA21CD","CHGIND")]
  lookup_lsoa_2011_21 = lookup_lsoa_2011_21[!duplicated(lookup_lsoa_2011_21$LSOA21CD),]

  lookup_dz_2011_22$warningcode = 1L

  lookup_lsoa_2011_21$warningcode = ifelse(lookup_lsoa_2011_21$CHGIND == "X", 2L, NA_integer_)
  lookup_lsoa_2011_21$warningcode = ifelse(lookup_lsoa_2011_21$CHGIND == "M", 3L, lookup_lsoa_2011_21$warningcode)
  lookup_lsoa_2011_21$warningcode = ifelse(lookup_lsoa_2011_21$CHGIND == "S", 4L, lookup_lsoa_2011_21$warningcode)

  lookup_lsoa_2011_21$CHGIND = NULL

  population = population[,c("LSOA21CD","all_ages")]
  population = population[population$all_ages == 0,]
  population = population[!duplicated(population$LSOA21CD),]
  population$warningcode = 5L
  population$all_ages = NULL

  # Company cars: taken straight from the flag combine_lsoa_emissions() sets
  # when it blanks a zone-year's company/bike emissions, so a zone can never be
  # suppressed in the data without the warning appearing on its report card
  company = data.frame(
    LSOA21CD = unique(lsoa_emissions_all$LSOA21CD[lsoa_emissions_all$company_bike_suppressed]),
    warningcode = 6L
  )

  warningcode = rbind(lookup_dz_2011_22, lookup_lsoa_2011_21, population, company)
  warningcode = warningcode[order(warningcode$LSOA21CD),]

  # Bespoke warnings. The hand-curated code 6 list is kept as well as the
  # derived one above: it flags zones whose fleet registrations are known to be
  # wrong but which fall below the suppression threshold. Codes derived twice
  # are dropped here rather than shown twice on the report card.

  extra = read.csv("data/lsoa_warnings.csv")
  warningcode = rbind(warningcode, extra)
  warningcode = warningcode[!duplicated(warningcode[,c("LSOA21CD","warningcode")]),]


  warningcode
}
