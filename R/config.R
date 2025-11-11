#' Riverfly species and water quality reading type mappings
#' A set of named lists that map internal column names to human-readable names for Riverfly species and water quality reading types.
#' These mappings are used throughout the package for data processing and visualization.
#' @format A list of named lists:
#' \describe{
#' \item{riverfly_spp_bw}{A named list mapping Riverfly species internal column names to human-readable names.}
#' \item{other_spp_bw}{A named list mapping other species internal column names to human-readable names.}
#' \item{water_quality_bw}{A named list mapping water quality reading type internal column
#' names to human-readable names.}
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

setNames(names(riverfly_spp_bw), riverfly_spp_bw)
setNames(names(other_spp_bw), other_spp_bw)

palette_for_leaflet <- RColorBrewer::brewer.pal(n = 9, name = "Blues")

#' plot_breaks
#'
#'
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
