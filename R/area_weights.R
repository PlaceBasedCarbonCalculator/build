# Population weights for splitting LSOAs between administrative areas.
#
# The area summaries (R/la_summaries.R, R/area_summaries.R) are all built up
# from per-LSOA data, so they need to know which area each LSOA belongs to.
# `lsoa_admin` answers that by putting each LSOA wholly in whichever area
# contains its population-weighted centroid. That is fine for large areas but
# fails badly for small ones: an LSOA holds around 1,500 people and a rural
# parish often holds far fewer, so one LSOA routinely covers several parishes
# and the centroid lands in only one of them. On the 2023 boundaries that left
# 5,901 of 11,344 parishes (and 43 English and Welsh wards) with no data at all.
#
# These functions replace the all-or-nothing assignment with a population
# weighting: each LSOA is split between the areas it overlaps in proportion to
# how many of its residents live in each. The population comes from the 2021
# census counts for Output Areas (`population_oa21`), which are roughly five
# times smaller than an LSOA, spread within each OA over its live postcodes
# (OS Code-Point Open, the `postcode_points` target). Postcodes are what make
# the last mile work: OA centroids alone still miss 1,112 parishes, because a
# sparse moorland parish shares its OA with a village that pulls the centroid
# away, whereas postcodes reach all but the 12 parishes that have no addresses
# in them at all.
#
# Weights are a fixed spatial pattern derived from the 2021 census and are
# applied to every year of data. That assumes the distribution of people within
# an LSOA has not changed much over time, which is a far smaller assumption
# than the one it replaces.

#' Split each LSOA's population between the areas it overlaps
#'
#' @description Builds the weighted LSOA-to-area lookup described above for one
#'   area type. Every live postcode is given an equal share of its Output
#'   Area's 2021 census population, located by its Code-Point position, and the
#'   shares falling inside each area are totalled. The weight is that total as
#'   a fraction of the LSOA's whole postcode population, so an LSOA split
#'   between three parishes contributes three part-rows that sum to its whole.
#'
#'   Population that falls outside any area of this type keeps its share of the
#'   denominator rather than being redistributed: an LSOA that is half
#'   unparished gives its parish half its emissions, not all of them.
#'
#'   Scotland has no 2021 OA census counts (and no parishes), so Scottish zones
#'   fall back to the whole-zone assignment in `lsoa_admin` with a weight of 1.
#'   England and Wales zones with no matched postcode population fall back the
#'   same way.
#' @param bounds_area Area boundaries, an sf polygon layer whose first column
#'   is the area code (`bounds_wards` or `bounds_parish`).
#' @param area_col Name of the area code column (e.g. "WD25CD", "PAR23CD").
#' @param postcode_points Live GB postcode points (`postcode_points` target).
#' @param lookup_postcode_OA_LSOA_MSOA_2021 Postcode-to-OA/LSOA lookup.
#' @param population_oa21 2021 census population by Output Area.
#' @param lsoa_admin Zone-to-admin lookup (`lsoa_admin` target), used for the
#'   Scottish and unmatched fallback.
#' @return A data frame of `LSOA21CD`, the area code column, and `weight`, the
#'   share of that LSOA's population living in that area. Weights sum to at
#'   most 1 per LSOA (exactly 1 where the area type covers the whole country).
#'   Areas with no resident population produce no rows.
#' @keywords internal
lsoa_area_weights = function(bounds_area, area_col, postcode_points,
                             lookup_postcode_OA_LSOA_MSOA_2021, population_oa21,
                             lsoa_admin){

  pc = postcode_area_population(postcode_points, lookup_postcode_OA_LSOA_MSOA_2021,
                                population_oa21)

  bounds_area = bounds_area[, area_col]
  if(sf::st_crs(pc) != sf::st_crs(bounds_area)){
    bounds_area = sf::st_transform(bounds_area, sf::st_crs(pc))
  }
  pc = sf::st_join(pc, bounds_area)
  pc = sf::st_drop_geometry(pc)

  # Denominator is the LSOA's whole postcode population, including the part
  # outside any area of this type (unparished land), so shares aren't inflated
  totals = pc |>
    dplyr::group_by(LSOA21CD) |>
    dplyr::summarise(lsoa_pop = sum(pc_pop, na.rm = TRUE), .groups = "drop")

  weights = pc[!is.na(pc[[area_col]]), ] |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("LSOA21CD", area_col)))) |>
    dplyr::summarise(area_pop = sum(pc_pop, na.rm = TRUE), .groups = "drop")

  weights = dplyr::left_join(weights, totals, by = "LSOA21CD")
  weights$weight = weights$area_pop / weights$lsoa_pop
  weights = weights[is.finite(weights$weight) & weights$weight > 0, ]
  weights = weights[, c("LSOA21CD", area_col, "weight")]

  # Scotland, and any zone the postcode population missed, keep the
  # whole-zone assignment from lsoa_admin
  fallback = lsoa_admin[!lsoa_admin$LSOA21CD %in% weights$LSOA21CD,
                        c("LSOA21CD", area_col)]
  # "Unparished" is lsoa_admin's marker for land in no parish, not a place
  fallback = fallback[!is.na(fallback[[area_col]]) &
                        fallback[[area_col]] != "Unparished", ]
  fallback$weight = 1

  weights = rbind(as.data.frame(weights), as.data.frame(fallback))
  weights[order(weights$LSOA21CD, weights[[area_col]]), ]
}

#' Locate the 2021 census population at postcode level
#'
#' @description Shared by `lsoa_area_weights()` for every area type: joins each
#'   live England and Wales postcode to its Output Area and LSOA, then gives it
#'   an equal share of that OA's 2021 census population. Splitting equally
#'   within an OA is an approximation, but an OA averages only about 315 people
#'   across roughly a dozen postcodes, so it is a fine-grained one.
#' @param postcode_points Live GB postcode points (`postcode_points` target).
#' @param lookup_postcode_OA_LSOA_MSOA_2021 Postcode-to-OA/LSOA lookup.
#' @param population_oa21 2021 census population by Output Area.
#' @return An sf POINT data frame with `LSOA21CD` and `pc_pop`.
#' @keywords internal
postcode_area_population = function(postcode_points, lookup_postcode_OA_LSOA_MSOA_2021,
                                    population_oa21){

  lookup = lookup_postcode_OA_LSOA_MSOA_2021[, c("pcds", "oa21cd", "lsoa21cd")]
  pc = dplyr::left_join(postcode_points, lookup, by = c("postcode" = "pcds"))

  # 2021 OA census counts are England & Wales only; Scotland is handled by the
  # lsoa_admin fallback in lsoa_area_weights()
  pc = pc[!is.na(pc$oa21cd) & grepl("^[EW]", pc$oa21cd), ]

  n_postcodes = pc |>
    sf::st_drop_geometry() |>
    dplyr::count(oa21cd, name = "n_postcodes")

  pc = dplyr::left_join(pc, n_postcodes, by = "oa21cd")
  pc = dplyr::left_join(pc, population_oa21[, c("OA21CD", "total_pop")],
                        by = c("oa21cd" = "OA21CD"))
  pc$pc_pop = pc$total_pop / pc$n_postcodes
  names(pc)[names(pc) == "lsoa21cd"] = "LSOA21CD"

  pc[!is.na(pc$LSOA21CD) & !is.na(pc$pc_pop), c("LSOA21CD", "pc_pop")]
}

#' Build the weighted LSOA-to-area lookups for every area level
#'
#' @description Assembles the list consumed by the area summary functions, one
#'   entry per level of `area_levels()`. Wards and parishes are population
#'   weighted by `lsoa_area_weights()`, because they are small enough for the
#'   whole-LSOA assignment to lose areas entirely. Local authorities and
#'   Westminster constituencies are far larger than an LSOA and already have
#'   complete coverage, so they keep the established whole-LSOA assignment from
#'   `lsoa_admin` (a weight of 1) and their published figures are unchanged.
#' @param lsoa_admin Zone-to-admin lookup (`lsoa_admin` target).
#' @param bounds_wards Ward boundaries (`bounds_wards` target).
#' @param bounds_parish Parish boundaries (`bounds_parish` target).
#' @param postcode_points Live GB postcode points (`postcode_points` target).
#' @param lookup_postcode_OA_LSOA_MSOA_2021 Postcode-to-OA/LSOA lookup.
#' @param population_oa21 2021 census population by Output Area.
#' @return A named list (`la`, `ward`, `parish`, `constituency`) of data frames
#'   with `LSOA21CD`, the level's area code column, and `weight`.
#' @keywords internal
build_area_weights = function(lsoa_admin, bounds_wards, bounds_parish,
                              postcode_points, lookup_postcode_OA_LSOA_MSOA_2021,
                              population_oa21){

  unweighted = function(area_col){
    out = lsoa_admin[!is.na(lsoa_admin[[area_col]]), c("LSOA21CD", area_col)]
    out$weight = 1
    as.data.frame(out)
  }

  list(
    la = unweighted("LAD25CD"),
    ward = lsoa_area_weights(bounds_wards, "WD25CD", postcode_points,
                             lookup_postcode_OA_LSOA_MSOA_2021, population_oa21,
                             lsoa_admin),
    parish = lsoa_area_weights(bounds_parish, "PAR23CD", postcode_points,
                               lookup_postcode_OA_LSOA_MSOA_2021, population_oa21,
                               lsoa_admin),
    constituency = unweighted("PCON24CD")
  )
}

#' Name every area that has data, disambiguating repeated names
#'
#' @description Lists the areas appearing in a weighted lookup, with the name
#'   from the boundary layer. Ward and parish names are shared by many
#'   different places ("Abbey" belongs to sixteen wards), so where a name
#'   occurs more than once the local authority holding most of the area's
#'   population is appended, as `lsoa_admin_summary()` does. A handful of
#'   parishes share both a name and a local authority (North Yorkshire absorbed
#'   several districts in 2023 and has three parishes called Aislaby), so any
#'   name still repeated after that also gets its Westminster constituency.
#'   Used by `build_area_names()`.
#' @param weights One level of `build_area_weights()`.
#' @param bounds_area Area boundaries with the code and name columns.
#' @param area_col Name of the area code column (e.g. "WD25CD").
#' @param name_col Name of the area name column (e.g. "WD25NM").
#' @param lsoa_admin Zone-to-admin lookup, for the qualifying area names.
#' @return A data frame of `id` and `name`, one row per area with data.
#' @keywords internal
area_names_with_data = function(weights, bounds_area, area_col, name_col, lsoa_admin){

  areas = sf::st_drop_geometry(bounds_area)[, c(area_col, name_col)]
  areas = areas[areas[[area_col]] %in% weights[[area_col]], ]
  names(areas) = c("id", "name")

  # An area's local authority (or constituency) is the one holding most of its
  # population; an area straddling a boundary is named for the side it mostly
  # sits in
  dominant = function(col){
    x = dplyr::left_join(weights, lsoa_admin[, c("LSOA21CD", col)], by = "LSOA21CD")
    x = x |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(area_col, col)))) |>
      dplyr::summarise(w = sum(weight, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(w)) |>
      dplyr::distinct(dplyr::across(dplyr::all_of(area_col)), .keep_all = TRUE)
    x[[col]][match(areas$id, x[[area_col]])]
  }

  # Qualifiers accumulate onto the boundary name rather than onto the last
  # attempt, so a parish whose own name ends in brackets ("Linton (Penyard
  # Ward)") keeps it
  qualifiers = list(dominant("LAD25NM"), dominant("PCON24NM"))
  base = areas$name
  qualified = rep(NA_character_, nrow(areas))
  for(q in qualifiers){
    repeated = areas$name %in% unique(areas$name[duplicated(areas$name)])
    if(!any(repeated)) break
    qualified[repeated] = ifelse(is.na(qualified[repeated]), q[repeated],
                                 paste0(qualified[repeated], ", ", q[repeated]))
    named = !is.na(qualified)
    areas$name[named] = paste0(base[named], " (", qualified[named], ")")
  }

  areas = areas[, c("id", "name")]
  areas[order(areas$name), ]
}

#' Canonical display names for wards and parishes
#'
#' @description One name per area, shared by everything the website shows: the
#'   search lists built by `RScripts/make_la_jsons.R`, the report page titles
#'   they feed, and the ward and parish an LSOA report links to. Deriving them
#'   in one place is what keeps those three in step, because whether a name
#'   needs its local authority appended depends on which other areas exist
#'   alongside it.
#' @param area_weights The `area_weights` target.
#' @param bounds_wards Ward boundaries (`bounds_wards` target).
#' @param bounds_parish Parish boundaries (`bounds_parish` target).
#' @param lsoa_admin Zone-to-admin lookup, for the local authority names.
#' @return A named list (`ward`, `parish`) of data frames with `id` and `name`.
#' @keywords internal
build_area_names = function(area_weights, bounds_wards, bounds_parish, lsoa_admin){
  list(
    ward = area_names_with_data(area_weights$ward, bounds_wards,
                                "WD25CD", "WD25NM", lsoa_admin),
    parish = area_names_with_data(area_weights$parish, bounds_parish,
                                  "PAR23CD", "PAR23NM", lsoa_admin)
  )
}
