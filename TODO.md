# TODOs from R/

This file aggregates `TODO` and `FIXME` comments found in the `R/` folder, with a short explanation of the work needed for each note.

> Generated from `R/*.R` TODO/`FIXME` comments on April 10, 2026.

## access_counts.R
- Line 108: `#TODO: Some places have inf accessibility e.g. E01027752 to bus stops`
  - Investigate why infinite accessibility values are produced and handle them cleanly in the accessibility calculation.

## anoymised_mot_data.R
- Line 19: `#   #TODO: Update to 2023`
  - Refresh the anonymised MOT data ingestion to include 2023 data and confirm all file paths and sheet names are still valid.

## car_emissions.R
- Line 61: `#TODO: no obvious way to split up average age and average CO2`
  - Determine a consistent method to separate average vehicle age and average CO2 values in the emissions data.

## combine_emissions.R
- Line 13: `#TODO: some NA grades, check for consistency of 0 population zones.`
  - Verify how missing grades are handled and ensure zones with zero population are consistent across emission joins.
- Line 23: `#names(consumption_emissions) = gsub("emisions","emissions",names(consumption_emissions)) #TODO Fix typo`
  - Fix the typo in the column renaming step or remove the deprecated cleanup line if no longer needed.

## community_photo_scot.R
- Line 342: `#TODO: Fix bug in int_trs when inputs are all interger to start with`
  - Debug and correct the integer rounding / transformation logic in `int_trs` for pure integer input vectors.

## consumption_footprint.R
- Line 175: `#TODO: Categories don't match so just use an overall weight`
  - Resolve the mismatch between category definitions and implement more accurate weighting logic.
- Line 229: `#   #TODO: weight as GB is not whole UK`
  - Adjust weighting so it accounts for the full UK rather than Great Britain only, if required by the dataset.
- Line 287: `#   #TODO: change income over time`
  - Add temporal income adjustment logic rather than treating income brackets as static.

## dz_11_to_22_lookup.R
- Line 180: `tax_sub$households = tax_sub$all_properties #TODO better estimate of households pre-2014`
  - Improve the household estimate for pre-2014 data rather than using `all_properties` as a proxy.

## family_portraits.R
- Line 69: `#TODO: earlier we pivot wide only to pivot back, that wastes about and hour!`
  - Refactor the data transformation pipeline to avoid unnecessary wide→long conversions and improve performance.

## flights.R
- Line 76: `#TODO: Get time series of British vs foreign residents`
  - Add a time series dataset or breakdown that separates British and foreign resident flight behavior.
- Line 91: `# TODO: Better way to split emissions between nations.`
  - Improve the method used to allocate flight emissions across nations.
- Line 193: `#   #TODO: Get Scotland Income`
  - Add Scotland-specific income data to the analysis.
- Line 194: `#   #TODO: Change income shares over time`
  - Model changing income shares over time instead of using a static income distribution.

## furness_balancing.R
- Line 15: `#TODO: Get Inf values if mat_rsum is zero`
  - Ensure that matrix balancing handles zero row sums without producing infinite values.

## households_scotland.R
- Line 19: `# TODO: Review notes with this file`
  - Review and verify this file's data assumptions and documentation to ensure correctness.

## income.R
- Line 3: `#TODO: Total Weakly Income or Net weekly Income / Equivalised  / Before / After Housing Costs`
  - Clarify and standardize which income measure should be used in the income processing functions.

## land_uses.R
- Line 3: `#TODO: landuse:farmyard is a residential land use that can overlap with non-residential`
  - Update land use categorization rules to handle farmyard overlaps correctly.
- Line 23: `#TODO: Add danger areas?`
  - Consider adding danger area mapping or classification to the land use dataset.
- Line 117: `# TODO: Move this upstream`
  - Refactor the processing step indicated so it occurs earlier in the workflow where appropriate.

## LCFS.R
- Line 67: `# TODO: about 30 codes missing from lookup`
  - Investigate and extend the lookup to cover missing LCFS codes.
- Line 75: `# TODO: 23405 purchases missing DivisionalDescription`
  - Investigate the missing `DivisionalDescription` values and decide whether to infer them or flag them.
- Line 161: `# TODO: Domestic flights does not record under of passengers so assuming single adult`
  - Improve the passenger assignment logic for domestic flight records.
- Line 579: `#TODO: Check that string are the same across years, which some are not`
  - Verify year-to-year string consistency in LCFS data and normalize mismatching labels.
- Line 673: `#TODO: OAC seem wrong in post 2020 data, emailed ONS`
  - Confirm OAC coding in post-2020 data and apply corrections if needed.
- Line 973: `#TODO: Documentation seems to be wrong for post 2022 as C73311t C73312t is specified but B487 B488 is in the data and makes the total numbers add up`
  - Reconcile LCFS documentation with actual post-2022 dataset fields.

## lsoa_gas_electricity.R
- Line 257: `#TODO: How do you get the median of a subgroup? For now assuming unchanged`
  - Find a method to derive subgroup medians or explain the assumption used in the current transformation.
- Line 343: `#TODO: How do you get the median of a subgroup? For now assuming unchanged`
  - Same as above for the second electric/gas update flow.
- Line 772: `# TODO: Missing Scotland data for 2023 and 2024`
  - Add or source the missing Scotland gas/electricity data for 2023 and 2024 if available.

## non_gas_heating_bills.R
- Line 1: `#TODO; This is a very rough estimate of energy spending on fuels other than`
  - Improve the estimate for non-gas heating fuel spending and document the assumptions.

## OAC_summary.R
- Line 2: `#TODO: Scotland`
  - Extend the OAC summary logic to support Scotland if the dataset requires it.

## os_zoomstack.R
- Line 19: `# TODO: Finish this function`
  - Complete the `zoomstack_buildings_high` function logic.
- Line 41: `# TODO: Finish this function`
  - Complete the `zoomstack_buildings_lsoa` function logic.
- Line 167: `# TODO: Finish this function`
  - Finish the `split_buildings` helper or remove if unused.
- Line 274: `# TODO: Finish this function`
  - Complete the remaining OS Zoomstack processing functions.

## pmtiles.R
- Line 38: `# TODO: check if extra spaces caused by collapse matter`
  - Verify whether extra spaces in the tile-building collapse operations affect output correctness.

## population_households_historical.R
- Line 14: `#TODO: Check odd LSOA resutls E01033274 E01024150 E01024301 E01016129 E01024504 W01001971`
  - Investigate the anomalous LSOA results listed and fix any data or logic issues.
- Line 26: `#TODO: Move this upstream`
  - Refactor the `ecode`→`LSOA21CD` rename to occur earlier in the pipeline, if possible.
- Line 261: `#TODO Some Checks go here`
  - Add validation or consistency checks at the location indicated.

## population_scotland.R
- Line 58: `#TODO: Get Scotland 2022 population`
  - Add Scotland 2022 population estimates to the population dataset.

## retrofit_lsoa_maps.R
- Line 66: `#TODO: Scotland`
  - Extend retrofit map processing to include Scotland-specific data.
- Line 98: `# TODO: 373 LSOAs have less than 5 transactions per year`
  - Handle or document the low-transaction LSOAs in the retrofit analysis.

## synth_pop_emissions.R
- Line 53: `#TODO: Need fix for missing pops in even year, e.g. firs year of population is 2019`
  - Fix the emission workflow when population data for even years is missing or incomplete.

## synth_pop_LCFS.R
- Line 85: `population = population[population$LSOA21CD %in% unique(census21_synth_households$LSOA21CD),] #TODO: Scotland`
  - Confirm whether Scotland should be included in the synthetic population filter and adapt the logic accordingly.
- Line 467: `# TODO: Missing in 2020 and 2021 data. - Removed by ONS`
  - Document or handle the missing feature in the 2020/2021 data if required.

## vehicle_reg_to_21.R
- Line 126: `#TODO: Fix for old Scotland 2011 Bounds`
  - Correct the logic for old Scotland 2011 boundaries in vehicle registration processing.
