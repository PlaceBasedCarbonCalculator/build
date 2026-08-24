# Synthetic census Scotland - v2 (fast) ---------------------------------------
#
# NEW CODE (2026-07): faster replacement for sythetic_census_scot() /
# scot_syth_combine() in synthetic_pop_scotland.R (which are unchanged, for
# cross-comparison). Differences from v1:
#
#  1. The four coarse classifications (CarVan3, Tenure3, AccType3,
#     householdComp4) are no longer seed dimensions. Each is a deterministic
#     grouping of its fine counterpart (CarVan5, Tenure5, AccType7,
#     householdComp10), recovered from the national seed's zero pattern.
#     The seed shrinks from 9-D / 945,000 cells to 5-D / 8,750 cells - a
#     108x reduction in the work done by every IPF sweep.
#  2. mipfp::Ipfp (pure-R apply() sweeps) is replaced by ipf_grouped()
#     (rowsum-based, see R/ipf_fast.R); coarse-category constraints are
#     applied as grouped margins. Same normalisation and update semantics.
#  3. tol loosens from mipfp's 1e-10 to 1e-6 (the Data Zone tables are
#     mutually inconsistent, so 1e-10 was never reached and every zone ran
#     the full 1000 iterations).
#  4. Validation MAEs and the final collapse to the five reporting
#     variables are computed on the array (grouped margin sums) instead of
#     group_by/pivot_wider over a 945,000-row data frame per zone.
#  5. int_trs() integerises the 8,750-cell table directly; v1 integerised
#     the 945,000-cell table and then summed. Same expectation, different
#     random draws, so per-zone counts differ by ~sampling noise.
#
# Output columns and row content match v1 (householdComp10, CarVan5,
# Tenure5, hhSize5, AccType7, households, error_margins, conv, MAE,
# LSOA21CD; households > 0 only). Row order differs.
#
# Suggested target definition (add alongside, do NOT replace, the v1 target
# while comparing):
#
# tar_target(scot_synth_households_v2,{
#   sythetic_census_scot_v2(path_data = file.path(parameters$path_data,"population_scotland"),
#                           synth_pop_seed_scotland)
# }),


#' Precompute seed and constraint group indices for the Scottish v2 IPF
#'
#' @description One-off preparation shared by every Data Zone: collapses
#'   the national 9-D seed to the 5 fine dimensions, recovers the four
#'   fine-to-coarse mappings from the seed's zero pattern (erroring if any
#'   is ambiguous), and builds a group-index spec for each of the fifteen
#'   constraint table layouts used by `scot_syth_combine_v2()`.
#' @param synth_pop_seed_scotland National seed data frame
#'   (`synth_pop_seed_scotland` target).
#' @return A list with `seed5` (numeric vector), `dim5`, `dn5` (dimnames)
#'   and `specs` (named list of group-index specs).
#' @keywords internal
prep_synth_seed_scot_v2 = function(synth_pop_seed_scotland){

  householdComp10 = c("OnePersonOver66","OnePersonOther","FamilyOver66","CoupleNoChildren","CoupleChildren","CoupleNonDepChildren","LoneParent","LoneParentNonDepChildren","OtherChildren","OtherIncStudentOrOver66")
  householdComp4 = c("oneperson","couplefamily","loneparentfamily","other")
  hhSize5 = c("p1","p2","p3","p4","p5+")
  Tenure3 = c("owned","socialrented","privaterented")
  Tenure5 = c("outright","mortgage","socialrented","privaterented","rentfree")
  CarVan5 = c("car0","car1","car2","car3","car4+")
  CarVan3 = c("car0","car1","car2+")
  AccType3 = c("house","flat","caravan")
  AccType7 = c("detached","semidetached","terraced","flatpurposebuilt","flatconverted","flatcommercial","caravan")

  # Same layout as v1:
  # householdComp10, CarVan5, CarVan3, Tenure5, Tenure3, hhSize5, AccType7, AccType3, householdComp4
  seed9 = array(synth_pop_seed_scotland$seed, dim = c(10,5,3,5,3,5,7,3,4),
                dimnames = list(householdComp10, CarVan5, CarVan3, Tenure5, Tenure3,
                                hhSize5, AccType7, AccType3, householdComp4))

  # Fine-category seed (off-mapping cells only hold the 1e-15 floor)
  seed5 = apply(seed9, c(1, 2, 4, 6, 7), sum)

  # Fine -> coarse mappings, recovered from the seed and verified
  map_carvan3  = derive_grouping(apply(seed9, c(2, 3), sum), what = "CarVan5->CarVan3")
  map_tenure3  = derive_grouping(apply(seed9, c(4, 5), sum), what = "Tenure5->Tenure3")
  map_acctype3 = derive_grouping(apply(seed9, c(7, 8), sum), what = "AccType7->AccType3")
  map_comp4    = derive_grouping(apply(seed9, c(1, 9), sum), what = "householdComp10->householdComp4")

  dim5 = dim(seed5)
  ci = arrayInd(seq_len(prod(dim5)), dim5)
  idx = list(
    householdComp10 = ci[, 1],
    CarVan5         = ci[, 2],
    Tenure5         = ci[, 3],
    hhSize5         = ci[, 4],
    AccType7        = ci[, 5],
    householdComp4  = unname(map_comp4[ci[, 1]]),
    CarVan3         = unname(map_carvan3[ci[, 2]]),
    Tenure3         = unname(map_tenure3[ci[, 3]]),
    AccType3        = unname(map_acctype3[ci[, 5]])
  )
  sizes = c(householdComp10 = 10L, CarVan5 = 5L, Tenure5 = 5L, hhSize5 = 5L,
            AccType7 = 7L, householdComp4 = 4L, CarVan3 = 3L, Tenure3 = 3L,
            AccType3 = 3L)

  gspec = function(dims){ipf_group_index(idx[dims], unname(sizes[dims]))}

  # Dimension orders must match the array_maker() calls in the worker
  specs = list(
    MCarVan                 = gspec(c("CarVan5","CarVan3")),
    MHouseholdComp          = gspec(c("householdComp4","householdComp10")),
    MTenure                 = gspec(c("Tenure3","Tenure5")),
    MAccType                = gspec(c("AccType3","AccType7")),
    MhhSize5_Tenure3        = gspec(c("hhSize5","Tenure3")),
    MhhSize5_householdComp4 = gspec(c("hhSize5","householdComp4")),
    MhhSize5_AccType3       = gspec(c("AccType3","hhSize5")),
    MCarVan5_AccType3       = gspec(c("AccType3","CarVan5")),
    MCarVan5_Tenure3        = gspec(c("Tenure3","CarVan5")),
    MhouseholdComp4_CarVan3 = gspec(c("CarVan3","householdComp4")),
    MhouseholdComp4_AccType3 = gspec(c("AccType3","householdComp4")),
    MhouseholdComp4_Tenure3 = gspec(c("Tenure3","householdComp4")),
    MhhSize5_CarVan3        = gspec(c("CarVan3","hhSize5")),
    MTenure5_AccType3       = gspec(c("AccType3","Tenure5")),
    MTenure5_CarVan3        = gspec(c("CarVan3","Tenure5"))
  )

  list(seed5 = as.numeric(seed5), dim5 = dim5, dn5 = dimnames(seed5),
       specs = specs)
}


#' Build the Scottish synthetic household population (v2, fast)
#'
#' @description Faster reimplementation of `sythetic_census_scot()` - see
#'   the header of this file for the differences. Loads the same fifteen
#'   Data Zone cross-tab extracts, splits them per zone and runs
#'   `scot_syth_combine_v2()` in parallel.
#' @param path_data Folder of Scotland Census 2022 extracts.
#' @param synth_pop_seed_scotland National seed
#'   (`synth_pop_seed_scotland` target).
#' @param iter Maximum IPF iterations per zone (v1 used mipfp's default
#'   1000).
#' @param tol IPF stopping tolerance (v1 used mipfp's 1e-10).
#' @param zone_subset Optional character vector of Data Zone codes to build
#'   only a subset of zones (for testing / comparison against v1).
#' @param workers Number of parallel workers (default: future's default).
#' @return A data frame of household counts per Data Zone and attribute
#'   combination, with convergence diagnostics; columns as v1.
#' @keywords internal
sythetic_census_scot_v2 = function(path_data = file.path(parameters$path_data,"population_scotland"),
                                   synth_pop_seed_scotland,
                                   iter = 1000, tol = 1e-6,
                                   zone_subset = NULL, workers = NULL){

  # Load Data (same extracts as v1)
  dz_CarVan = read_CarVan_scot(file.path(path_data,"scotlandcenus2022_CarVan5_CarVan3_DataZone.csv"))
  dz_HouseholdComp = read_hhComp_scot(file.path(path_data,"scotlandcenus2022_householdComp10_householdComp4_DataZone.csv"))
  dz_Tenure = read_Tenure_scot(file.path(path_data,"scotlandcenus2022_Tenure5_Tenure3_DataZone.csv"))
  dz_AccType = read_Acc_scot(file.path(path_data,"scotlandcenus2022_AccType7_AccType3_DataZone.csv"))
  dz_hhSize5_Tenure3 = read_hhSize5_Tenure3_scot(file.path(path_data,"scotlandcenus2022_hhSize5_Tenure3_DataZone.csv"))
  dz_hhSize5_householdComp4 = read_hhSize5_householdComp4_scot(file.path(path_data,"scotlandcenus2022_hhSize5_householdComp4_DataZone.csv"))
  dz_hhSize5_AccType3 = read_hhSize5_AccType3_scot(file.path(path_data,"scotlandcenus2022_hhSize5_AccType3_DataZone.csv"))
  dz_CarVan5_AccType3 = read_CarVan5_AccType3_scot(file.path(path_data,"scotlandcenus2022_CarVan5_AccType3_DataZone.csv"))
  dz_CarVan5_Tenure3 = read_CarVan5_Tenure3_scot(file.path(path_data,"scotlandcenus2022_CarVan5_Tenure3_DataZone.csv"))
  dz_householdComp4_CarVan3 = read_householdComp4_CarVan3_scot(file.path(path_data,"scotlandcenus2022_householdComp4_CarVan3_DataZone.csv"))
  dz_householdComp4_AccType3 = read_householdComp4_AccType3_scot(file.path(path_data,"scotlandcenus2022_householdComp4_AccType3_DataZone.csv"))
  dz_householdComp4_Tenure3 = read_householdComp4_Tenure3_scot(file.path(path_data,"scotlandcenus2022_householdComp4_Tenure3_DataZone.csv"))
  dz_hhSize5_CarVan3 = read_hhSize5_CarVan3_scot(file.path(path_data,"scotlandcenus2022_hhSize5_CarVan3_DataZone.csv"))
  dz_Tenure5_AccType3 = read_Tenure5_AccType3_scot(file.path(path_data,"scotlandcenus2022_Tenure5_AccType3_DataZone.csv"))
  dz_Tenure5_CarVan3 = read_Tenure5_CarVan3_scot(file.path(path_data,"scotlandcenus2022_Tenure5_CarVan3_DataZone.csv"))

  tabs = list(dz_CarVan = dz_CarVan,
              dz_HouseholdComp = dz_HouseholdComp,
              dz_Tenure = dz_Tenure,
              dz_AccType = dz_AccType,
              dz_hhSize5_Tenure3 = dz_hhSize5_Tenure3,
              dz_hhSize5_householdComp4 = dz_hhSize5_householdComp4,
              dz_hhSize5_AccType3 = dz_hhSize5_AccType3,
              dz_CarVan5_AccType3 = dz_CarVan5_AccType3,
              dz_CarVan5_Tenure3 = dz_CarVan5_Tenure3,
              dz_householdComp4_CarVan3 = dz_householdComp4_CarVan3,
              dz_householdComp4_AccType3 = dz_householdComp4_AccType3,
              dz_householdComp4_Tenure3 = dz_householdComp4_Tenure3,
              dz_hhSize5_CarVan3 = dz_hhSize5_CarVan3,
              dz_Tenure5_AccType3 = dz_Tenure5_AccType3,
              dz_Tenure5_CarVan3 = dz_Tenure5_CarVan3)

  if(!is.null(zone_subset)){
    tabs = lapply(tabs, function(x){x[x$LSOA21CD %in% zone_subset, ]})
    if(nrow(tabs[[1]]) == 0){stop("zone_subset matches no zone codes")}
  }

  # Order and split per zone; check the fifteen tables cover identical
  # zone sets (v1 relied on this implicitly, v2 verifies)
  tabs = lapply(tabs, function(x){
    x = x[order(x$LSOA21CD), ]
    dplyr::group_split(dplyr::ungroup(x), LSOA21CD)
  })
  zone_sets = lapply(tabs, function(x){vapply(x, function(y){y$LSOA21CD[1]}, "")})
  if(length(unique(zone_sets)) != 1){
    stop("The fifteen Data Zone tables do not cover identical zone sets")
  }

  prep = prep_synth_seed_scot_v2(synth_pop_seed_scotland)

  if(is.null(workers)){
    future::plan("multisession")
  } else {
    future::plan("multisession", workers = workers)
  }
  res_all = furrr::future_pmap(.l = unname(tabs),
                               .f = scot_syth_combine_v2,
                               prep = prep,
                               iter = iter,
                               tol = tol,
                               .progress = TRUE,
                               .options = furrr::furrr_options(seed = 1234))
  future::plan("sequential")
  res_all = dplyr::bind_rows(res_all)

  res_all
}


#' IPF one Data Zone's household cross-tab (v2, grouped IPF)
#'
#' @description Worker for `sythetic_census_scot_v2()`. Same constraint
#'   arrays, weighting and integerisation as `scot_syth_combine()`, but
#'   fits only the 5 fine dimensions with `ipf_grouped()` (coarse-category
#'   constraints become grouped margins) and validates via grouped margin
#'   sums.
#' @param dz_CarVan_sub,dz_HouseholdComp_sub,dz_Tenure_sub,dz_AccType_sub,dz_hhSize5_Tenure3_sub,dz_hhSize5_householdComp4_sub,dz_hhSize5_AccType3_sub,dz_CarVan5_AccType3_sub,dz_CarVan5_Tenure3_sub,dz_householdComp4_CarVan3_sub,dz_householdComp4_AccType3_sub,dz_householdComp4_Tenure3_sub,dz_hhSize5_CarVan3_sub,dz_Tenure5_AccType3_sub,dz_Tenure5_CarVan3_sub
#'   One zone's rows of each census cross-tab.
#' @param prep Precomputed seed/spec list from `prep_synth_seed_scot_v2()`.
#' @param iter Maximum IPF iterations.
#' @param tol IPF stopping tolerance.
#' @return A one-zone data frame of household counts per attribute
#'   combination with convergence diagnostics.
#' @keywords internal
scot_syth_combine_v2 = function(dz_CarVan_sub,
                                dz_HouseholdComp_sub,
                                dz_Tenure_sub,
                                dz_AccType_sub,
                                dz_hhSize5_Tenure3_sub,
                                dz_hhSize5_householdComp4_sub,
                                dz_hhSize5_AccType3_sub,
                                dz_CarVan5_AccType3_sub,
                                dz_CarVan5_Tenure3_sub,
                                dz_householdComp4_CarVan3_sub,
                                dz_householdComp4_AccType3_sub,
                                dz_householdComp4_Tenure3_sub,
                                dz_hhSize5_CarVan3_sub,
                                dz_Tenure5_AccType3_sub,
                                dz_Tenure5_CarVan3_sub,
                                prep,
                                iter = 1000,
                                tol = 1e-6) {

  # Check Zone match
  if(length(unique(c(dz_CarVan_sub$LSOA21CD,
                     dz_HouseholdComp_sub$LSOA21CD,
                     dz_Tenure_sub$LSOA21CD,
                     dz_AccType_sub$LSOA21CD,
                     dz_hhSize5_Tenure3_sub$LSOA21CD,
                     dz_hhSize5_householdComp4_sub$LSOA21CD,
                     dz_hhSize5_AccType3_sub$LSOA21CD,
                     dz_CarVan5_AccType3_sub$LSOA21CD,
                     dz_CarVan5_Tenure3_sub$LSOA21CD,
                     dz_householdComp4_CarVan3_sub$LSOA21CD,
                     dz_householdComp4_AccType3_sub$LSOA21CD,
                     dz_householdComp4_Tenure3_sub$LSOA21CD,
                     dz_hhSize5_CarVan3_sub$LSOA21CD,
                     dz_Tenure5_AccType3_sub$LSOA21CD,
                     dz_Tenure5_CarVan3_sub$LSOA21CD
  ))) != 1){
    stop("More than one LSOA21CD")
  }

  householdComp10 = c("OnePersonOver66","OnePersonOther","FamilyOver66","CoupleNoChildren","CoupleChildren","CoupleNonDepChildren","LoneParent","LoneParentNonDepChildren","OtherChildren","OtherIncStudentOrOver66")
  householdComp4 = c("oneperson","couplefamily","loneparentfamily","other")
  hhSize5 = c("p1","p2","p3","p4","p5+")
  Tenure3 = c("owned","socialrented","privaterented")
  Tenure5 = c("outright","mortgage","socialrented","privaterented","rentfree")
  CarVan5 = c("car0","car1","car2","car3","car4+")
  CarVan3 = c("car0","car1","car2+")
  AccType3 = c("house","flat","caravan")
  AccType7 = c("detached","semidetached","terraced","flatpurposebuilt","flatconverted","flatcommercial","caravan")

  # Constraint arrays (identical dimension orders to v1)
  MCarVan = array_maker(dz_CarVan_sub, CarVan5, CarVan3)
  MHouseholdComp = array_maker(dz_HouseholdComp_sub,householdComp4, householdComp10)
  MTenure = array_maker(dz_Tenure_sub,Tenure3,Tenure5)
  MAccType = array_maker(dz_AccType_sub,AccType3, AccType7)
  MhhSize5_Tenure3 = array_maker(dz_hhSize5_Tenure3_sub,hhSize5, Tenure3)
  MhhSize5_householdComp4 = array_maker(dz_hhSize5_householdComp4_sub,hhSize5, householdComp4)
  MhhSize5_AccType3 = array_maker(dz_hhSize5_AccType3_sub,AccType3, hhSize5)
  MCarVan5_AccType3 = array_maker(dz_CarVan5_AccType3_sub,AccType3,CarVan5)
  MCarVan5_Tenure3 = array_maker(dz_CarVan5_Tenure3_sub,Tenure3,CarVan5)
  MhouseholdComp4_CarVan3 = array_maker(dz_householdComp4_CarVan3_sub,CarVan3, householdComp4)
  MhouseholdComp4_AccType3 = array_maker(dz_householdComp4_AccType3_sub,AccType3, householdComp4)
  MhouseholdComp4_Tenure3 = array_maker(dz_householdComp4_Tenure3_sub,Tenure3, householdComp4)
  MhhSize5_CarVan3 = array_maker(dz_hhSize5_CarVan3_sub, CarVan3, hhSize5)
  MTenure5_AccType3 = array_maker(dz_Tenure5_AccType3_sub, AccType3, Tenure5)
  MTenure5_CarVan3 = array_maker(dz_Tenure5_CarVan3_sub, CarVan3, Tenure5)

  med_pop = median(c(sum(MCarVan),
                     sum(MHouseholdComp),
                     sum(MTenure),
                     sum(MAccType),
                     sum(MhhSize5_Tenure3),
                     sum(MhhSize5_householdComp4),
                     sum(MhhSize5_AccType3),
                     sum(MCarVan5_AccType3),
                     sum(MCarVan5_Tenure3),
                     sum(MhouseholdComp4_CarVan3),
                     sum(MhouseholdComp4_AccType3),
                     sum(MhouseholdComp4_Tenure3),
                     sum(MhhSize5_CarVan3),
                     sum(MTenure5_AccType3),
                     sum(MTenure5_CarVan3)
  ))

  seed_weighted = (prep$seed5 / sum(prep$seed5)) * med_pop

  # Constraints in the same order mipfp applied them in v1
  targets = list(
    MCarVan                 = MCarVan,
    MHouseholdComp          = MHouseholdComp,
    MTenure                 = MTenure,
    MAccType                = MAccType,
    MhhSize5_Tenure3        = MhhSize5_Tenure3,
    MhhSize5_householdComp4 = MhhSize5_householdComp4,
    MhhSize5_AccType3       = MhhSize5_AccType3,
    MCarVan5_AccType3       = MCarVan5_AccType3,
    MCarVan5_Tenure3        = MCarVan5_Tenure3,
    MhouseholdComp4_CarVan3 = MhouseholdComp4_CarVan3,
    MhouseholdComp4_AccType3 = MhouseholdComp4_AccType3,
    MhouseholdComp4_Tenure3 = MhouseholdComp4_Tenure3,
    MhhSize5_CarVan3        = MhhSize5_CarVan3,
    MTenure5_AccType3       = MTenure5_AccType3,
    MTenure5_CarVan3        = MTenure5_CarVan3
  )

  res = ipf_grouped(seed_weighted,
                    target.data = targets,
                    group.list = prep$specs[names(targets)],
                    iter = iter, tol = tol)

  res2 = int_trs(array(res$x.hat * med_pop, dim = prep$dim5, dimnames = prep$dn5))

  # Integrity checks: same fourteen constraints as v1
  chk1 = grouped_mae(res2, prep$specs$MhouseholdComp4_CarVan3, MhouseholdComp4_CarVan3)
  chk2 = grouped_mae(res2, prep$specs$MhhSize5_householdComp4, MhhSize5_householdComp4)
  chk3 = grouped_mae(res2, prep$specs$MCarVan5_Tenure3, MCarVan5_Tenure3)
  chk4 = grouped_mae(res2, prep$specs$MTenure5_AccType3, MTenure5_AccType3)
  chk5 = grouped_mae(res2, prep$specs$MCarVan, MCarVan)
  chk6 = grouped_mae(res2, prep$specs$MHouseholdComp, MHouseholdComp)
  chk7 = grouped_mae(res2, prep$specs$MTenure, MTenure)
  chk8 = grouped_mae(res2, prep$specs$MAccType, MAccType)
  chk9 = grouped_mae(res2, prep$specs$MhhSize5_Tenure3, MhhSize5_Tenure3)
  chk10 = grouped_mae(res2, prep$specs$MhhSize5_AccType3, MhhSize5_AccType3)
  chk11 = grouped_mae(res2, prep$specs$MhouseholdComp4_AccType3, MhouseholdComp4_AccType3)
  chk12 = grouped_mae(res2, prep$specs$MhouseholdComp4_Tenure3, MhouseholdComp4_Tenure3)
  chk13 = grouped_mae(res2, prep$specs$MhhSize5_CarVan3, MhhSize5_CarVan3)
  chk14 = grouped_mae(res2, prep$specs$MCarVan5_AccType3, MCarVan5_AccType3)

  result_df <- as.data.frame.table(res2)
  names(result_df) = c("householdComp10", "CarVan5", "Tenure5", "hhSize5", "AccType7", "households")

  result_df = result_df[result_df$households > 0,]
  result_df$error_margins = max(res$error.margins)
  result_df$conv = res$conv
  result_df$MAE = max(chk1,chk2,chk3,chk4,chk5,chk6,chk7,chk8,chk9,chk10,chk11,chk12,chk13,chk14)
  result_df$LSOA21CD = dz_CarVan_sub$LSOA21CD[1]

  result_df
}
