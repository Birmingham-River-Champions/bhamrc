#' Create empty submissions dataframe
#'
#' @return A tibble with all submission columns defined as character vectors.
create_blank_submission_df <- function() {
  cols <- c(
    "timestamp",
    "email_address",
    "organisation",
    "survey_date",
    "data_type",
    "sampling_site",
    "cased_caddisfly",
    "caseless_caddisfly",
    "olive_mayfly",
    "blue_winged_olive_mayfly",
    "freshwater_shrimp",
    "freshwater_hoglouse",
    "blackfly_larvae",
    "freshwater_worm",
    "freshwater_leech",
    "freshwater_snail",
    "freshwater_beetle",
    "green_drake_mayfly",
    "flat_bodied_stone_clinger_mayfly",
    "stonefly_plecoptera",
    "other_chironomidae",
    "other_dicranota",
    "other_tipulidae",
    "other_hydracarina",
    "other_hydropsychidae",
    "other_rhyacophilidae",
    "other_planorbidae",
    "other_sphaeriidae",
    "other_acroloxidae_ancylidae",
    "other_bullhead",
    "other_unspecified_1",
    "other_unspecified_2",
    "other_unspecified_3",
    "other_unspecified_4",
    "other_unspecified_5",
    "other_unspecified_6",
    "other_unspecified_7",
    "other_unspecified_8",
    "names_of_other_taxa",
    "sheet",
    "conductivity_mS",
    "temperature_C",
    "ammonia_ppm",
    "phosphate_ppm",
    "nitrate_ppm",
    "turbidity_NTU",
    "other_water_quality",
    "invasive_spp_sampling_date",
    "invasive_spp_wtw",
    "signal_crayfish",
    "killer_demon_shrimp",
    "himalayan_balsam",
    "japanese_knotweed",
    "giant_hogweed",
    "any_other_invasive_spp",
    "outfall_survey_date",
    "outfall_photo",
    "outfall_flow",
    "outfall_pollution_distance",
    "outfall_aesthetics",
    "other_pollution_description",
    "X",
    "Y"
  )

  col_list <- setNames(
    replicate(length(cols), character(), simplify = FALSE),
    cols
  )

  # Override X and Y to be numeric
  col_list$X <- numeric()
  col_list$Y <- numeric()

  tibble::as_tibble(col_list)
}
