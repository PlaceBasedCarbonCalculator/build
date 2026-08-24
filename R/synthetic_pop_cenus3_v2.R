# Synthetic census E&W - v2 (fast) -------------------------------------------
#
# NEW CODE (2026-07): faster replacement for sythetic_census() /
# census_syth_combine_v4() in synthetic_pop_cenus3.R (which are unchanged,
# for cross-comparison). Differences from v1:
#
#  1. hhComp6 is no longer a seed dimension. It is a deterministic grouping
#     of hhComp15 (see the `rules` list in build_synth_pop_seed()), so the
#     6-D 17,600-cell seed collapses to a 5-D 3,520-cell seed and hhComp6
#     constraints are applied as grouped margins via ipf_grouped().
#  2. mipfp::Ipfp (pure-R apply() sweeps) is replaced by ipf_grouped()
#     (rowsum-based, see R/ipf_fast.R) with the same update/normalisation
#     semantics.
#  3. iter drops from 20000 to 2000 and tol loosens from 1e-10 to 1e-6.
#     The zone tables are perturbed by ONS disclosure control so they are
#     mutually inconsistent and 1e-10 is never reached - v1 burned the full
#     20000 iterations per zone for no accuracy gain.
#  4. Validation MAEs are computed by grouped margin sums instead of
#     group_by/pivot_wider on the expanded data frame (same metric,
#     computed by position rather than name order).
#  5. The integerisation (int_trs) runs on the 3,520-cell array rather than
#     the 17,600-cell one; v1 integerised then summed out hhComp6, v2
#     integerises the already-collapsed table. Same expectation, different
#     random draws, so per-zone counts differ by ~sampling noise.
#
# Output columns and row content match v1 (hhSize5, CarVan5, Tenure5,
# AccType5, hhComp15, households, error_margins, conv, MAE, LSOA21CD;
# households > 0 only). Row order differs (array order, not group_by order).
#
# Suggested target definition (add alongside, do NOT replace, the v1 target
# while comparing):
#
# tar_target(census21_synth_households_v2,{
#   sythetic_census_v2(path = file.path(parameters$path_data,"population"),
#                      synth_pop_seed)
# }),


#' Precompute seed and constraint group indices for the E&W v2 IPF
#'
#' @description One-off preparation shared by every LSOA: collapses the
#'   national 6-D seed to the 5 fine dimensions (summing out hhComp6),
#'   recovers the hhComp15 -> hhComp6 mapping from the seed, and builds a
#'   group-index spec (see `ipf_group_index()`) for each of the fourteen
#'   constraint table layouts used by `census_syth_combine_v5()`.
#' @param synth_pop_seed National seed data frame (`synth_pop_seed` target).
#' @return A list with `seed5` (numeric vector), `dim5`, `dn5` (dimnames)
#'   and `specs` (named list of group-index specs).
#' @keywords internal
prep_synth_seed_ew_v2 = function(synth_pop_seed){

  hhComp15 = c("OnePersonOver66","OnePersonOther","FamilyOver66","CoupleNoChildren","CoupleChildren","CoupleNonDepChildren","LoneParent","LoneParentNonDepChildren","OtherChildren","OtherNoChildren","OtherIncStudentOrOver66")
  hhComp6 = c("OnePerson","FamilyOver66","CoupleFamily","LoneParent","Other6")
  hhSize5 = c("p1","p2","p3","p4+")
  Tenure5 = c("outright","mortgage","socialrented","privaterented")
  CarVan5 = c("car0","car1","car2","car3+")
  AccType5 = c("detached","semidetached","terraced","flat","caravan")

  # Same layout as v1: hhSize5, CarVan5, Tenure5, AccType5, hhComp6, hhComp15
  dim6 = c(length(hhSize5), length(CarVan5), length(Tenure5),
           length(AccType5), length(hhComp6), length(hhComp15))
  seed6 = array(synth_pop_seed$seed, dim = dim6,
                dimnames = list(hhSize5, CarVan5, Tenure5, AccType5, hhComp6, hhComp15))

  # Fine-category seed: sum out hhComp6 (off-mapping cells only hold the
  # 1e-15 floor, so this is effectively a re-indexing, not a smoothing)
  seed5 = apply(seed6, c(1, 2, 3, 4, 6), sum)

  # hhComp15 -> hhComp6 mapping, recovered from the seed and verified
  map_comp6 = derive_grouping(apply(seed6, c(6, 5), sum), what = "hhComp15->hhComp6")

  dim5 = dim(seed5)
  ci = arrayInd(seq_len(prod(dim5)), dim5)
  idx = list(
    hhSize5  = ci[, 1],
    CarVan5  = ci[, 2],
    Tenure5  = ci[, 3],
    AccType5 = ci[, 4],
    hhComp15 = ci[, 5],
    hhComp6  = unname(map_comp6[ci[, 5]])
  )
  sizes = c(hhSize5 = length(hhSize5), CarVan5 = length(CarVan5),
            Tenure5 = length(Tenure5), AccType5 = length(AccType5),
            hhComp15 = length(hhComp15), hhComp6 = length(hhComp6))

  gspec = function(dims){ipf_group_index(idx[dims], unname(sizes[dims]))}

  # Dimension orders must match the array_maker() calls in the worker
  specs = list(
    Acc_tenure            = gspec(c("AccType5","Tenure5")),
    hhComp_Tenure         = gspec(c("Tenure5","hhComp15")),
    Acc_hhComp6           = gspec(c("AccType5","hhComp6")),
    Acc_CarVan            = gspec(c("AccType5","CarVan5")),
    hhSize_hhComp         = gspec(c("hhComp6","hhSize5")),
    Acc_hhSize            = gspec(c("hhSize5","AccType5")),
    hhSize_CarVan         = gspec(c("hhSize5","CarVan5")),
    CarVan_hhComp         = gspec(c("hhComp6","CarVan5")),
    hhComp6_Tenure        = gspec(c("hhComp6","Tenure5")),
    Acc_hhComp            = gspec(c("AccType5","hhComp15")),
    Tenure_hhSize_CarVan  = gspec(c("Tenure5","hhSize5","CarVan5")),
    Tenure_CarVan_hhComp6 = gspec(c("hhComp6","CarVan5","Tenure5")),
    Tenure_hhSize         = gspec(c("Tenure5","hhSize5")),
    Tenure_CarVan         = gspec(c("Tenure5","CarVan5"))
  )

  list(seed5 = as.numeric(seed5), dim5 = dim5, dn5 = dimnames(seed5),
       specs = specs)
}


#' Build the E&W synthetic household population (v2, fast)
#'
#' @description Faster reimplementation of `sythetic_census()` - see the
#'   header of this file for the differences. Loads the same fourteen census
#'   extracts, splits them per LSOA and runs `census_syth_combine_v5()` in
#'   parallel.
#' @param path_data Folder of census 2021 extracts.
#' @param synth_pop_seed National seed (`synth_pop_seed` target).
#' @param iter Maximum IPF iterations per zone (v1 used 20000).
#' @param tol IPF stopping tolerance (v1 used mipfp's 1e-10).
#' @param zone_subset Optional character vector of LSOA21CD codes to build
#'   only a subset of zones (for testing / comparison against v1).
#' @param workers Number of parallel workers (default: future's default).
#' @return A data frame of household counts per zone and attribute
#'   combination, with convergence diagnostics; columns as v1.
#' @keywords internal
sythetic_census_v2 = function(path_data = file.path(parameters$path_data,"population"),
                              synth_pop_seed,
                              iter = 2000, tol = 1e-6,
                              zone_subset = NULL, workers = NULL){

  Acc_hhComp = read_Acc_hhComp(file.path(path_data,"census2021EW_Households_HouseholdComposition15_AccomodationType5_LSOA_partial.csv"))  # Partial
  Acc_hhComp6 = read_Acc_hhComp6(file.path(path_data,"census2021EW_Households_HouseholdComposition6_AccType5_LSOA_partial.csv"))  # Partial
  Acc_CarVan = read_Acc_CarVan(file.path(path_data,"census2021EW_Households_AccomodationType5_CarVan5_LSOA_partial.csv"))  # Partial
  hhSize_hhComp = read_hhSize_hhComp(file.path(path_data,"census2021EW_Households_HouseholdComposition6_HouseholdSize5_LSOA_partial.csv"))  # Partial
  Tenure_hhSize_CarVan = read_Tenure_hhSize_CarVan(file.path(path_data,"census2021EW_Households_Tenure5_HouseholdSize5_CarVan5_LSOA_partial.csv")) # Partial
  Acc_hhSize = read_Acc_hhSize(file.path(path_data,"census2021EW_Households_AccomodationType5_HousehholdSize5_LSOA_partial.csv")) # Partial
  Tenure_CarVan_hhComp6 = read_Tenure_CarVan_hhComp6(file.path(path_data,"census2021EW_Households_Tenure5_HouseholdComposition6_CarVan5_LSOA_partial.csv")) # Partial
  hhSize_CarVan = read_hhSize_CarVan(file.path(path_data,"census2021EW_Households_hhSize5_CarVan5_LSOA_partial.csv")) # Partial

  Acc_tenure = read_Acc_tenure(file.path(path_data,"census2021EW_Households_AccomodationType5_Tenure5_LSOA.csv"))
  hhComp_Tenure = read_hhComp_Tenure(file.path(path_data,"census2021EW_Households_HouseholdComposition15_Tenure5_LSOA.csv"))
  hhComp6_Tenure = read_hhComp6_Tenure(file.path(path_data,"census2021EW_Households_Tenure5_HouseholdComposition6_LSOA.csv"))
  Tenure_hhSize = read_Tenure_hhSize(file.path(path_data,"census2021EW_Households_Tenure5_HouseholdSize5_LSOA.csv"))
  CarVan_hhComp = read_CarVan_hhComp(file.path(path_data,"census2021EW_Households_HouseholdComposition6_CarVan5_LSOA.csv"))
  Tenure_CarVan = read_Tenure_CarVan(file.path(path_data,"census2021EW_Households_Tenure5_CarVan5_LSOA.csv"))

  lsoa_all = unique(Acc_tenure$LSOA21CD)
  lsoa_all = lsoa_all[order(lsoa_all)]
  if(!is.null(zone_subset)){
    lsoa_all = lsoa_all[lsoa_all %in% zone_subset]
    if(length(lsoa_all) == 0){stop("zone_subset matches no LSOA21CD")}
  }

  Acc_tenure = split_for_arrays3(Acc_tenure, lsoa_all)
  hhComp_Tenure = split_for_arrays3(hhComp_Tenure, lsoa_all)
  Tenure_hhSize_CarVan = split_for_arrays3(Tenure_hhSize_CarVan, lsoa_all)
  Acc_hhComp = split_for_arrays3(Acc_hhComp, lsoa_all)
  Acc_hhComp6 = split_for_arrays3(Acc_hhComp6, lsoa_all)
  Acc_CarVan = split_for_arrays3(Acc_CarVan, lsoa_all)
  hhSize_hhComp = split_for_arrays3(hhSize_hhComp, lsoa_all)
  Acc_hhSize = split_for_arrays3(Acc_hhSize, lsoa_all)
  hhComp6_Tenure = split_for_arrays3(hhComp6_Tenure, lsoa_all)
  Tenure_hhSize = split_for_arrays3(Tenure_hhSize, lsoa_all)
  CarVan_hhComp = split_for_arrays3(CarVan_hhComp, lsoa_all)
  Tenure_CarVan = split_for_arrays3(Tenure_CarVan, lsoa_all)
  Tenure_CarVan_hhComp6 = split_for_arrays3(Tenure_CarVan_hhComp6, lsoa_all)
  hhSize_CarVan = split_for_arrays3(hhSize_CarVan, lsoa_all)

  prep = prep_synth_seed_ew_v2(synth_pop_seed)

  if(is.null(workers)){
    future::plan("multisession")
  } else {
    future::plan("multisession", workers = workers)
  }
  res_2 = furrr::future_pmap(.l = list(Acc_tenure_sub = Acc_tenure,
                                       hhComp_Tenure_sub = hhComp_Tenure,
                                       Tenure_hhSize_CarVan_sub = Tenure_hhSize_CarVan,
                                       Acc_hhComp_sub = Acc_hhComp,
                                       Acc_hhComp6_sub = Acc_hhComp6,
                                       Acc_CarVan_sub = Acc_CarVan,
                                       hhSize_hhComp_sub = hhSize_hhComp,
                                       Acc_hhSize_sub = Acc_hhSize,
                                       hhComp6_Tenure_sub = hhComp6_Tenure,
                                       CarVan_hhComp_sub = CarVan_hhComp,
                                       Tenure_CarVan_hhComp6_sub = Tenure_CarVan_hhComp6,
                                       Tenure_hhSize_sub = Tenure_hhSize,
                                       Tenure_CarVan_sub = Tenure_CarVan,
                                       hhSize_CarVan_sub = hhSize_CarVan
                                       ),
                             .f = census_syth_combine_v5,
                             prep = prep,
                             iter = iter,
                             tol = tol,
                             .progress = TRUE,
                             .options = furrr::furrr_options(seed = 1234))
  future::plan("sequential")
  res_2 = dplyr::bind_rows(res_2)

  res_2
}


#' IPF one LSOA's household cross-tab (v2, grouped IPF)
#'
#' @description Worker for `sythetic_census_v2()`. Same constraint
#'   preferences, zone checks, weighting and integerisation as
#'   `census_syth_combine_v4()`, but fits the 5 fine dimensions with
#'   `ipf_grouped()` and validates via grouped margin sums.
#' @param Acc_tenure_sub,hhComp_Tenure_sub,Tenure_hhSize_CarVan_sub,Acc_hhComp_sub,Acc_hhComp6_sub,Acc_CarVan_sub,hhSize_hhComp_sub,Acc_hhSize_sub,hhComp6_Tenure_sub,CarVan_hhComp_sub,Tenure_CarVan_hhComp6_sub,Tenure_hhSize_sub,Tenure_CarVan_sub,hhSize_CarVan_sub
#'   One zone's rows of each census cross-tab (NULL where unpublished).
#' @param prep Precomputed seed/spec list from `prep_synth_seed_ew_v2()`.
#' @param iter Maximum IPF iterations.
#' @param tol IPF stopping tolerance.
#' @return A one-zone data frame of household counts per attribute
#'   combination with convergence diagnostics.
#' @keywords internal
census_syth_combine_v5 = function(Acc_tenure_sub,
                                  hhComp_Tenure_sub,
                                  Tenure_hhSize_CarVan_sub,
                                  Acc_hhComp_sub,
                                  Acc_hhComp6_sub,
                                  Acc_CarVan_sub,
                                  hhSize_hhComp_sub,
                                  Acc_hhSize_sub,
                                  hhComp6_Tenure_sub,
                                  CarVan_hhComp_sub,
                                  Tenure_CarVan_hhComp6_sub,
                                  Tenure_hhSize_sub,
                                  Tenure_CarVan_sub,
                                  hhSize_CarVan_sub,
                                  prep,
                                  iter = 2000,
                                  tol = 1e-6) {

  # Prefer multivariate to bivariate versions (as v1)
  if(!is.null(Tenure_hhSize_CarVan_sub)){
    Tenure_hhSize_sub <- NULL
    Tenure_CarVan_sub <- NULL
    hhSize_CarVan_sub <- NULL
  }
  if(!is.null(Tenure_CarVan_hhComp6_sub)){
    hhComp6_Tenure_sub <- NULL
    Tenure_CarVan_sub <- NULL
    CarVan_hhComp_sub <- NULL
  }

  lsoa_check = c(
     if(!is.null(Acc_tenure_sub)){Acc_tenure_sub$LSOA21CD},
     if(!is.null(hhComp_Tenure_sub)){hhComp_Tenure_sub$LSOA21CD},
     if(!is.null(Tenure_hhSize_CarVan_sub)){Tenure_hhSize_CarVan_sub$LSOA21CD},
     if(!is.null(Acc_hhComp_sub)){Acc_hhComp_sub$LSOA21CD},
     if(!is.null(Acc_hhComp6_sub)){Acc_hhComp6_sub$LSOA21CD},
     if(!is.null(Acc_CarVan_sub)){Acc_CarVan_sub$LSOA21CD},
     if(!is.null(hhSize_hhComp_sub)){hhSize_hhComp_sub$LSOA21CD},
     if(!is.null(Acc_hhSize_sub)){Acc_hhSize_sub$LSOA21CD},
     if(!is.null(hhComp6_Tenure_sub)){hhComp6_Tenure_sub$LSOA21CD},
     if(!is.null(CarVan_hhComp_sub)){CarVan_hhComp_sub$LSOA21CD},
     if(!is.null(Tenure_CarVan_hhComp6_sub)){Tenure_CarVan_hhComp6_sub$LSOA21CD},
     if(!is.null(Tenure_hhSize_sub)){Tenure_hhSize_sub$LSOA21CD},
     if(!is.null(Tenure_CarVan_sub)){Tenure_CarVan_sub$LSOA21CD},
     if(!is.null(hhSize_CarVan_sub)){hhSize_CarVan_sub$LSOA21CD}
  )
  if(length(unique(lsoa_check)) != 1){
    stop("More than one LSOA")
  }
  lsoacd = unique(lsoa_check)

  hhComp15 = c("OnePersonOver66","OnePersonOther","FamilyOver66","CoupleNoChildren","CoupleChildren","CoupleNonDepChildren","LoneParent","LoneParentNonDepChildren","OtherChildren","OtherNoChildren","OtherIncStudentOrOver66")
  hhComp6 = c("OnePerson","FamilyOver66","CoupleFamily","LoneParent","Other6")
  hhSize5 = c("p1","p2","p3","p4+")
  Tenure5 = c("outright","mortgage","socialrented","privaterented")
  CarVan5 = c("car0","car1","car2","car3+")
  AccType5 = c("detached","semidetached","terraced","flat","caravan")

  # Make arrays (identical dimension orders to v1)
  if(!is.null(Acc_tenure_sub)){
    Acc_tenure_sub = array_maker(Acc_tenure_sub, AccType5, Tenure5)
  }
  if(!is.null(hhComp_Tenure_sub)){
    hhComp_Tenure_sub = array_maker(hhComp_Tenure_sub, Tenure5,  hhComp15)
  }
  if(!is.null(Acc_hhComp6_sub)){
    Acc_hhComp6_sub = array_maker(Acc_hhComp6_sub, AccType5, hhComp6)
  }
  if(!is.null(Acc_CarVan_sub)){
    Acc_CarVan_sub = array_maker(Acc_CarVan_sub, AccType5, CarVan5)
  }
  if(!is.null(hhSize_hhComp_sub)){
    hhSize_hhComp_sub = array_maker(hhSize_hhComp_sub, hhComp6, hhSize5)
  }
  if(!is.null(Acc_hhSize_sub)){
    Acc_hhSize_sub = array_maker(Acc_hhSize_sub, hhSize5, AccType5)
  }
  if(!is.null(hhSize_CarVan_sub)){
    hhSize_CarVan_sub = array_maker(hhSize_CarVan_sub, hhSize5, CarVan5)
  }
  if(!is.null(CarVan_hhComp_sub)){
    CarVan_hhComp_sub = array_maker(CarVan_hhComp_sub, hhComp6, CarVan5)
  }
  if(!is.null(hhComp6_Tenure_sub)){
    hhComp6_Tenure_sub = array_maker(hhComp6_Tenure_sub, hhComp6, Tenure5)
  }
  if(!is.null(Acc_hhComp_sub)){
    Acc_hhComp_sub = array_maker(Acc_hhComp_sub, AccType5, hhComp15)
  }
  if(!is.null(Tenure_hhSize_CarVan_sub)){
    Tenure_hhSize_CarVan_sub = array_maker(Tenure_hhSize_CarVan_sub, Tenure5, hhSize5, CarVan5)
  }
  if(!is.null(Tenure_CarVan_hhComp6_sub)){
    Tenure_CarVan_hhComp6_sub = array_maker(Tenure_CarVan_hhComp6_sub, hhComp6, CarVan5, Tenure5)
  }
  if(!is.null(Tenure_hhSize_sub)){
    Tenure_hhSize_sub = array_maker(Tenure_hhSize_sub, Tenure5, hhSize5)
  }
  if(!is.null(Tenure_CarVan_sub)){
    Tenure_CarVan_sub = array_maker(Tenure_CarVan_sub, Tenure5, CarVan5)
  }

  med_pop = median(
    c(if(!is.null(Acc_tenure_sub)){sum(Acc_tenure_sub)},
     if(!is.null(hhComp_Tenure_sub)){sum(hhComp_Tenure_sub)},
     if(!is.null(Tenure_hhSize_CarVan_sub)){sum(Tenure_hhSize_CarVan_sub)},
     if(!is.null(Acc_hhComp_sub)){sum(Acc_hhComp_sub)},
     if(!is.null(Acc_hhComp6_sub)){sum(Acc_hhComp6_sub)},
     if(!is.null(Acc_CarVan_sub)){sum(Acc_CarVan_sub)},
     if(!is.null(hhSize_hhComp_sub)){sum(hhSize_hhComp_sub)},
     if(!is.null(Acc_hhSize_sub)){sum(Acc_hhSize_sub)},
     if(!is.null(hhComp6_Tenure_sub)){sum(hhComp6_Tenure_sub)},
     if(!is.null(CarVan_hhComp_sub)){sum(CarVan_hhComp_sub)},
     if(!is.null(Tenure_CarVan_hhComp6_sub)){sum(Tenure_CarVan_hhComp6_sub)},
     if(!is.null(Tenure_hhSize_sub)){sum(Tenure_hhSize_sub)},
     if(!is.null(Tenure_CarVan_sub)){sum(Tenure_CarVan_sub)},
     if(!is.null(hhSize_CarVan_sub)){sum(hhSize_CarVan_sub)}
  ))

  seed_weighted = (prep$seed5 / sum(prep$seed5)) * med_pop

  # Constraints in the same order mipfp applied them in v1
  avail = list(
    Acc_tenure            = Acc_tenure_sub,
    hhComp_Tenure         = hhComp_Tenure_sub,
    Tenure_hhSize_CarVan  = Tenure_hhSize_CarVan_sub,
    Acc_hhComp            = Acc_hhComp_sub,
    Acc_CarVan            = Acc_CarVan_sub,
    hhSize_hhComp         = hhSize_hhComp_sub,
    Acc_hhSize            = Acc_hhSize_sub,
    hhComp6_Tenure        = hhComp6_Tenure_sub,
    CarVan_hhComp         = CarVan_hhComp_sub,
    Acc_hhComp6           = Acc_hhComp6_sub,
    Tenure_CarVan_hhComp6 = Tenure_CarVan_hhComp6_sub,
    Tenure_hhSize         = Tenure_hhSize_sub,
    Tenure_CarVan         = Tenure_CarVan_sub,
    hhSize_CarVan         = hhSize_CarVan_sub
  )
  avail = avail[!vapply(avail, is.null, TRUE)]

  res = ipf_grouped(seed_weighted,
                    target.data = avail,
                    group.list = prep$specs[names(avail)],
                    iter = iter, tol = tol)

  res2 = int_trs(array(res$x.hat * med_pop, dim = prep$dim5, dimnames = prep$dn5))

  # Mean Absolute Error, same eleven checks as v1 (2D inputs only)
  chk_of = function(nm, arr){
    if(is.null(arr)){0} else {grouped_mae(res2, prep$specs[[nm]], arr)}
  }
  chk1 = chk_of("Acc_tenure", Acc_tenure_sub)
  chk2 = chk_of("hhComp_Tenure", hhComp_Tenure_sub)
  chk3 = chk_of("Acc_hhComp", Acc_hhComp_sub)
  chk4 = chk_of("Acc_CarVan", Acc_CarVan_sub)
  chk5 = chk_of("hhSize_hhComp", hhSize_hhComp_sub)
  chk6 = chk_of("Acc_hhSize", Acc_hhSize_sub)
  chk7 = chk_of("hhComp6_Tenure", hhComp6_Tenure_sub)
  chk8 = chk_of("CarVan_hhComp", CarVan_hhComp_sub)
  chk9 = chk_of("Acc_hhComp6", Acc_hhComp6_sub)
  chk10 = chk_of("Tenure_hhSize", Tenure_hhSize_sub)
  chk11 = chk_of("Tenure_CarVan", Tenure_CarVan_sub)

  result_df <- as.data.frame.table(res2)
  names(result_df) = c("hhSize5", "CarVan5", "Tenure5", "AccType5", "hhComp15", "households")

  result_df = result_df[result_df$households > 0,]
  result_df$error_margins = max(res$error.margins)
  result_df$conv = res$conv
  result_df$MAE = max(chk1,chk2,chk3,chk4,chk5,chk6,chk7,chk8,chk9,chk10,chk11)
  result_df$LSOA21CD = lsoacd

  result_df
}
