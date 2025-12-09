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
    "conductivity_mS" = "Conductivity (\u03BCS)",
    "temperature_C" = "Temperature (\u00B0C)",
    "ammonia_ppm" = "Ammonia (ppm)",
    "phosphate_ppm" = "Phosphate (ppm)",
    "nitrate_ppm" = "Nitrate (ppm)",
    "turbidity_NTU" = "Turbidity (NTU)"
)

data_types_bw <- list(
    "Urban Riverfly" = "riverfly",
    "Water Quality" = "water_quality",
    "Invasive Species" = "invasive_species",
    "Riverfly Locations" = "riverfly_locs",
    "Urban Outfall Safari" = "outfall_safari"
)

unspecified_bw <- setNames(
    paste0("Other Unspecified ", 1:8),
    paste0("other_unspecified_", 1:8)
)

# Custom column names for displaying data tables
column_names <- list(
    "riverfly" = c(
        organisation = "Organisation",
        survey_date = "Survey Date",
        data_type = "Data Type",
        sampling_site = "Sampling Site",
        riverfly_spp_bw,
        other_spp_bw,
        unspecified_bw,
        names_of_other_taxa = "Names of Other Taxa"
    ),
    "water_quality" = c(
        organisation = "Organisation",
        survey_date = "Survey Date",
        data_type = "Data Type",
        sampling_site = "Sampling Site",
        water_quality_bw,
        other_water_quality = "Other Water Quality Comments"
    ),
    "invasive_species" = c(
        organisation = "Organisation",
        survey_date = "Survey Date",
        data_type = "Data Type",
        invasive_spp_sampling_date = "Invasive Species Sampling Date",
        sampling_site = "Sampling Site",
        invasive_spp_wtw = "What3Words Location",
        signal_crayfish = "Signal Crayfish Count",
        killer_demon_shrimp = "Killer/Demon Shrimp Count",
        himalayan_balsam = "Himalayan Balsam Prevalence",
        japanese_knotweed = "Japanese Knotweed Prevalence",
        giant_hogweed = "Giant Hogweed Prevalence",
        any_other_invasive_spp = "Any Other Invasive Species"
    ),
    "outfall_safari" = c(
        organisation = "Organisation",
        survey_date = "Survey Date",
        data_type = "Data Type",
        outfall_survey_date = "Outfall Survey Date",
        sampling_site = "Sampling Site",
        outfall_photo = "Outfall Photo Uploaded",
        outfall_flow = "Outfall Flow",
        outfall_pollution_distance = "Outfall Pollution Distance",
        outfall_aesthetics = "Outfall Aesthetics",
        other_pollution_description = "Other Pollution Description"
    )
)

setNames(names(riverfly_spp_bw), riverfly_spp_bw)
setNames(names(other_spp_bw), other_spp_bw)
setNames(names(water_quality_bw), water_quality_bw)

#Palette to avoid duplication for blues plots
palette_for_leaflet <- RColorBrewer::brewer.pal(n = 9, name = "Blues")

# CSS to make required fields have a red star
appCSS <- ".mandatory_star { color: red; }"

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
    invasive_spp_wtw = "What.three.words location of invasive species seen 'out and about' - (separate with period - e.g., \"above.awake.nature\") If you don't have the app, go to what3words.com, and select the geolocate button (you may have to allow your location to be identified). Click on other nearby squares if the GPS isn't accurate enough.",
    killer_demon_shrimp = "Number of killer / demon shrimp counted in a kick sample - leave blank if 0",
    signal_crayfish = "Number of signal crayfish counted in a kick sample / observed on riverbed - leave blank if 0",
    outfall_photo = "Have you uploaded a photo of the outfall <a href = 'https://www.dropbox.com/request/J0BAKMCneqB698oF7xJD'>here</a>
     (please use the <b>outfall location ID</b> as the picture caption)? <font color='red'>*</font>",
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
    other_unspecified_1 = "Additional taxa not in Urban Riverfly. If 'other taxa', please specify the name and abundance below."
)


#' Text for the choices for water quality, outfall, and abundance form inputs.
choices_list <- list(
    "Phosphate (ppm)" = c(
        "Not measured",
        0.02,
        0.05,
        0.1,
        0.2,
        0.5,
        1
    ),
    "Nitrate (ppm)" = c(
        "Not measured",
        0.2,
        0.5,
        1,
        2,
        5,
        10
    ),
    "Turbidity (NTU)" = c(
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
    ),
    abundance = c(
        "0",
        "1-9",
        "10-99",
        "100 - 999",
        "1000+"
    ),
    invasive_flora = c(
        "Not observed",
        "Present (1-33%)",
        "Abundant (>33%)"
    )
)

# This is the ID for the new sheet with four separate tables
google_sheet_id <-
    "https://docs.google.com/spreadsheets/d/1jRIIBVBYvEJNkgIcEqnOmFHn4bw7Syimw_4Ad3lV7XY/edit?pli=1&gid=2035392267#gid=2035392267"
