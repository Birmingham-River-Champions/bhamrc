#' Riverfly species and water quality reading type mappings
#' A set of named lists that map internal column names to human-readable names for Riverfly species and water quality reading types.
#' These mappings are used throughout the package for data processing and visualization.
#' @format A list of named lists:
#' \describe{
#' \item{riverfly_spp_bw}{A named list mapping Riverfly species internal column names to human-readable names.}
#' \item{other_spp_bw}{A named list mapping other species internal column names to human-readable names.}
#' \item{water_quality_bw}{A named list mapping water quality reading type internal column}
#' \item{plot_palette}{A vector of colors used for plotting water quality data.}
#' \item{plot_breaks}{A named vector mapping Riverfly species internal column names to human-readable names.}
#' }
riverfly_spp_bw <- list(
    "cased_caddisfly" = "Cased caddisfly (Trichoptera)",
    "caseless_caddisfly" = "Caseless caddisfly (Trichoptera)",
    "olive_mayfly" = "Olive mayfly (Baetidae)",
    "blue_winged_olive_mayfly" = "Blue-winged olive mayfly (Ephemerellidae)",
    "freshwater_shrimp" = "Freshwater shrimp (Gammaridae)",
    "freshwater_hoglouse" = "Freshwater hoglouse (Asellidae)",
    "blackfly_larvae" = "Blackfly larvae (Simuliidae)",
    "freshwater_worm" = "Freshwater worm (Oligochaeta)",
    "freshwater_leech" = "Freshwater leech (Hirudinea)",
    "freshwater_snail" = "Freshwater snail (Gastropoda)",
    "freshwater_beetle" = "Freshwater beetle (Coleoptera)",
    "green_drake_mayfly" = "Green drake mayfly (Ephemeridae)",
    "flat_bodied_stone_clinger_mayfly" = "Flat-bodied stone clinger mayfly (Heptageniidae)",
    "stonefly_plecoptera" = "Stonefly larvae (Plecoptera)"
)

other_spp_bw <- list(
    "other_chironomidae" = "Non-biting midge larvae (Chironomidae)",
    "other_dicranota" = "Cranefly larvae (Dicranota sp.)",
    "other_tipulidae" = "Other cranefly larvae (Tipulidae)",
    "other_hydracarina" = "Water mite (Hydracarina)",
    "other_hydropsychidae" = "Net spinning (caseless) caddisfly (Hydropsychidae)",
    "other_rhyacophilidae" = "Green sedge (caseless) caddisfly (Rhyacophilidae)",
    "other_planorbidae" = "Ramshorn snail (Planorbidae)",
    "other_sphaeriidae" = "Freshwater mollusc (Sphaeriidae)",
    "other_acroloxidae_ancylidae" = "Freshwater limpet (Acroloxidae/Ancylidae)",
    "other_bullhead" = "Bullhead (fish - Cottus gobio)"
)

water_quality_bw <- list(
    "Conductivity (\u03BCS)" = "conductivity_mS",
    "Temperature (\u00B0C)" = "temperature_C",
    "Ammonia (ppm)" = "ammonia_ppm",
    "Phosphate (ppm)" = "phosphate_ppm",
    "Nitrate (ppm)" = "nitrate_ppm",
    "Turbidity (NTU)" = "turbidity_NTU"
)

data_types_bw <- list(
    "Urban Riverfly" = "riverfly",
    "Water Quality" = "water_quality",
    "Invasive Species" = "invasive_species",
    "Riverfly Locations" = "riverfly_locs",
    "Urban Outfall Safari" = "outfall_safari"
)

setNames(names(riverfly_spp_bw), riverfly_spp_bw)
setNames(names(other_spp_bw), other_spp_bw)

palette_for_leaflet <- RColorBrewer::brewer.pal(n = 9, name = "Blues")

#' plot_breaks
#'
#' These are the breaks used for plotting water quality and ARMI data.
#' They define the bins for categorizing readings into different levels.
plot_breaks <- data.frame(
    reading_type = c(
        rep("ARMI", 6),
        rep("conductivity_mS", 6),
        rep("ammonia_ppm", 6),
        rep("phosphate_ppm", 6),
        rep("nitrate_ppm", 6),
        rep("turbidity_NTU", 6),
        rep("temperature_C", 6)
    ),
    bin_breaks = c(
        -Inf,
        5,
        8,
        11,
        14,
        Inf,
        -Inf,
        350,
        450,
        550,
        650,
        Inf,
        -Inf,
        .05,
        .15,
        .25,
        .58,
        Inf,
        -Inf,
        0.05,
        0.1,
        0.2,
        0.5,
        Inf,
        -Inf,
        0.5,
        1,
        2,
        5,
        Inf,
        -Inf,
        17,
        25,
        40,
        100,
        Inf,
        -Inf,
        5,
        10,
        15,
        25,
        Inf
    ),
    bin = rep(1:6, 7)
)

#' Question wording for survey input form
#' A named list mapping internal column names to human-readable question wording for use in the data entry form.
#' This is used to provide clear labels for form fields when users are entering data.
survey_questions <- list(
    organisation = "Organisation Name",
    survey_date = "Survey Date",
    data_type = "Data Type",
    sampling_site = "Sampling Site",
    cased_caddisfly = "Number of cased caddisfly (Trichoptera) - leave blank if none observed",
    caseless_caddisfly = "Number of caseless caddisfly (Trichoptera) - leave blank if none observed",
    olive_mayfly = "Number of olive mayfly (Baetidae) - leave blank if none observed",
    blue_winged_olive_mayfly = "Number of blue-winged olive mayflies (Ephemerellidae) - leave blank if none observed",
    freshwater_shrimp = "Number of freshwater shrimp (Gammaridae) - leave blank if none observed",
    freshwater_hoglouse = "Number of Freshwater hoglouse (Asellidae) - leave blank if none observed",
    blackfly_larvae = "Number of Blackfly larvae (Simuliidae) - leave blank if none observed",
    freshwater_worm = "Number of Freshwater worm (Oligochaeta) - leave blank if none observed",
    freshwater_leech = "Number of Freshwater leech (Hirudinea) - leave blank if none observed",
    freshwater_snail = "Number of Freshwater snail (Gastropoda) - leave blank if none observed",
    freshwater_beetle = "Number of Freshwater beetle (Coleoptera) - leave blank if none observed",
    green_drake_mayfly = "Number of Green drake mayfly (Ephemeridae) - leave blank if none observed",
    flat_bodied_stone_clinger_mayfly = "Number of Flat-bodied stone clinger mayfly (Heptageniidae) - leave blank if none observed",
    stonefly_plecoptera = "Number of Stonefly larvae (Plecoptera) - leave blank if none observed",
    other_chironomidae = "Number of Non-biting midge larvae (Chironomidae) - leave blank if none observed",
    other_dicranota = "Number of Cranefly larvae (Dicranota sp.) - leave blank if none observed",
    other_tipulidae = "Number of Other cranefly larvae (Tipulidae) - leave blank if none observed",
    other_hydracarina = "Number of Water mite (Hydracarina) - leave blank if none observed",
    other_hydropsychidae = "Number of Net spinning (caseless) caddisfly (Hydropsychidae) - leave blank if none observed",
    other_rhyacophilidae = "Number of Green sedge (caseless) caddisfly (Rhyacophilidae) - leave blank if none observed",
    other_planorbidae = "Number of Ramshorn snail (Planorbidae) - leave blank if none observed",
    other_sphaeriidae = "Number of Freshwater mollusc (Sphaeriidae) - leave blank if none observed",
    other_acroloxidae_ancylidae = "Number of Freshwater limpet (Acroloxidae/Ancylidae) - leave blank if none observed",
    other_bullhead = "Number of Bullhead (fish - Cottus gobio) - leave blank if none observed",
    conductivity_mS = "Conductivity (\u03BCS)",
    temperature_C = "Temperature (\u00B0C)",
    ammonia_ppm = "Ammonia (ppm)",
    phosphate_ppm = "Phosphate (ppm)",
    nitrate_ppm = "Nitrate (ppm)",
    turbidity_NTU = "Turbidity (NTU)",
    other_water_quality = "Other water quality observations",
    outfall_survey_date = "TEXT",
    sampling_site = "TEXT",
    outfall_photo = "Have you uploaded a photo of the outfall here (please use the Outfall location ID as the picture caption)?",
    outfall_flow = "Flow from outfall",
    outfall_pollution_distance = "Pollution distance downstream from the outfall (meters)",
    outfall_aesthetics = "Outfall aesthetics"
)
