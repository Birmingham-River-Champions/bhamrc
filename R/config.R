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
    organisation = "Name of organisation",
    survey_date = "Date of survey",
    data_type = "Data type",
    sampling_site = "BRC sampling site ID",
    cased_caddisfly = "Number of cased caddisfly (Trichoptera) - leave blank if none observed",
    caseless_caddisfly = "Number of caseless caddisfly (Trichoptera) - leave blank if none observed",
    olive_mayfly = "Number of olive mayfly (Baetidae) - leave blank if none observed",
    blue_winged_olive_mayfly = "Number of blue-winged olive mayflies (Ephemerellidae) - leave blank if none observed",
    freshwater_shrimp = "Number of freshwater shrimp (Gammaridae) - leave blank if none observed",
    freshwater_hoglouse = "Number of freshwater hoglouse (Asellidae) - leave blank if none observed",
    blackfly_larvae = "Number of blackfly larvae (Simuliidae) - leave blank if none observed",
    freshwater_worm = "Number of freshwater worm (Oligochaeta) - leave blank if none observed",
    freshwater_leech = "Number of freshwater leech (Hirudinea) - leave blank if none observed",
    freshwater_snail = "Number of freshwater snail (Gastropoda) - leave blank if none observed",
    freshwater_beetle = "Number of freshwater beetle (Coleoptera) - leave blank if none observed",
    green_drake_mayfly = "Number of green drake mayfly (Ephemeridae) - leave blank if none observed",
    flat_bodied_stone_clinger_mayfly = "Number of flat-bodied stone clinger mayfly (Heptageniidae) - leave blank if none observed",
    stonefly_plecoptera = "Number of stonefly larvae (Plecoptera) - leave blank if none observed",
    other_chironomidae = "Number of non-biting midge larvae (Chironomidae) - leave blank if none observed",
    other_dicranota = "Number of cranefly larvae (Dicranota sp.) - leave blank if none observed",
    other_tipulidae = "Number of other cranefly larvae (Tipulidae) - leave blank if none observed",
    other_hydracarina = "Number of water mite (Hydracarina) - leave blank if none observed",
    other_hydropsychidae = "Number of net spinning (caseless) caddisfly (Hydropsychidae) - leave blank if none observed",
    other_rhyacophilidae = "Number of green sedge (caseless) caddisfly (Rhyacophilidae) - leave blank if none observed",
    other_planorbidae = "Number of ramshorn snail (Planorbidae) - leave blank if none observed",
    other_sphaeriidae = "Number of freshwater mollusc (Sphaeriidae) - leave blank if none observed",
    other_acroloxidae_ancylidae = "Number of freshwater limpet (Acroloxidae/Ancylidae) - leave blank if none observed",
    other_bullhead = "Number of bullhead (fish - Cottus gobio) - leave blank if none observed",
    conductivity_mS = "Conductivity (\u03BCS)",
    temperature_C = "Temperature (\u00B0C)",
    ammonia_ppm = "Ammonia (ppm)",
    phosphate_ppm = "Phosphate (ppm)",
    nitrate_ppm = "Nitrate (ppm)",
    turbidity_NTU = "Turbidity (NTU)",
    other_water_quality = "Other comments on water quality",
    invasive_spp_sampling_date = "Date of observation",
    invasive_spp_wtw = "What.three.words location of invasive species seen 'out and about' - (separate with period - e.g., \"above.awake.nature\") If you don't have the app, go to what3words.com, and select the geolocate button (you may have to allow your location to be identified). Click on other nearby squares if the GPS isn't accurate enough.",
    killer_demon_shrimp = "Number of killer / demon shrimp counted in a kick sample - leave blank if 0",
    signal_crayfish = "Number of signal crayfish counted in a kick sample / observed on riverbed - leave blank if 0",
    outfall_survey_date = "Date of survey",
    sampling_site = "Outfall ID",
    outfall_photo = "Have you uploaded a photo of the outfall <a href = 'https://www.dropbox.com/request/J0BAKMCneqB698oF7xJD'>here</a> (please use the <b>outfall location ID</b> as the picture caption)?",
    outfall_flow = "Flow from outfall",
    outfall_pollution_distance = "Pollution distance downstream from the outfall",
    outfall_aesthetics = "Outfall aesthetics",
    other_pollution_description = "Description of other pollution types (e.g., pollutant colour, food waste, and clogged debris) and other comments",
    himalayan_balsam = "Himalayan balsam prevalence over a 10-metre cross-section (approx. 10 strides) - leave blank if not observed",
    japanese_knotweed = "Japanese knotweed prevalence over a 10-metre cross-section (approx. 10 strides) - leave blank if not observed",
    giant_hogweed = "Giant hogweed prevalence over a 10-metre cross-section (approx. 10 strides) - leave blank if not observed",
    any_other_invasive_spp = "Note any invasive species or comments here (images of unidentifiable species should be emailed to birminghamriverchampions@gmail.com)",
    outfall_location_wtw = "Outfall location - What.three.words (separate with period - e.g., \"above.awake.nature\"). If you don't have the app, go to what3words.com, and select the geolocate button (you may have to allow your location to be identified).
     Click on other nearby squares if the GPS isn't accurate enough.",
    other_taxa_1 = "If 'other taxa' please specify the name and abundance below."
)

choices_list <- list(
    phosphate_ppm = c(
        "Not measured",
        0.02,
        0.05,
        0.1,
        0.2,
        0.5,
        1
    ),
    nitrate_ppm = c(
        "Not measured",
        0.2,
        0.5,
        1,
        2,
        5,
        10
    ),
    turbidity_NTU = c(
        "Not measured",
        14,
        15,
        17,
        19,
        21,
        25,
        30,
        35,
        40,
        50,
        75,
        100,
        150,
        200,
        240
    ),
    outfall_flow = c(
        "No flow",
        "Trickle: <0.1l/s, enough to fill a teacup in 1 minute",
        "Low flow: 0.1 - 1 l/s, enough to fill a bucket in 1 minute",
        "Moderate flow: 1 - 2 l/s, fills more than 1 bucket in 1 minute",
        "High flow: >2 l/s, clearly fills more than a bathtub in 1 minute"
    ),
    outfall_pollution_distance = c(
        "No visible effect",
        "Impact within 2m of outfall",
        "Impact 2m to 10m from outfall",
        "Impact 10m to 30m from outfall",
        "Impact greater than 30m from outfall"
    ),
    outfall_aesthetics = c(
        "No odour or visible aesthetics",
        "Faint smell, slight discolouration",
        "Mild smell, mild discolouration, small coverage of grey fungus",
        "Strong smell, strong discolouration, large coverage of grey fungus and/or litter",
        "Gross smell, gross sewage"
    )
)
abundance_choices <- c(
    "0",
    "1-9",
    "10-99",
    "100 - 999",
    "1000+"
)
