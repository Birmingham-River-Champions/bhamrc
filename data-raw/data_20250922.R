## code to prepare `data_20250922` dataset goes here

# Read in data from Google Sheets
full_form_url <- "https://docs.google.com/spreadsheets/d/1458OWr2_x3vdM_LGAQaf0lcOWitO9LtnRm2GsAF_pys/edit?usp=sharing"
BRC_full_form <- as.data.frame(gsheet::gsheet2tbl(full_form_url))

# Get rid of duplicate columns, spaces, and other odd characters in column names
names(BRC_full_form) <- c(
    "timestamp",
    "email_address",
    "organisation",
    "data_type",
    "sampling_site_riverfly",
    "survey_date",
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
    "second_data_type",
    "invasive_spp_sampling_date",
    "invasive_spp_sampling_site",
    "invasive_spp_what_three_words",
    "signal_crayfish",
    "killer_demon_shrimp",
    "himalayan_balsam",
    "japanese_knotweed",
    "giant_hogweed",
    "any_other_invasive_spp",
    "third_data_type",
    "wq_survey_date",
    "wq_sampling_site",
    "conductivity_mS",
    "temperature_C",
    "ammonia_ppm",
    "phosphate_ppm",
    "nitrate_ppm",
    "turbidity_NTU",
    "other_water_quality",
    "fourth_data_type",
    "outfall_survey_date",
    "outfall_sampling_site",
    "outfall_photo",
    "outfall_flow",
    "outfall_pollution_distance",
    "outfall_aesthetics",
    "other_pollution_description",
    "fifth_data_type",
    "submit_or_return",
    "other_comments_river",
    "outfall_location_wtw",
    "location_wtw"
)

# Create location data frames for the two different location tables
locations_list <- process_locations(
    sampling_locs_url = 'https://docs.google.com/spreadsheets/d/1ZEkLC3HBkB8SJynA3pHtmntMOiCT8p4e2BFNYsMUR4c/edit?usp=sharing',
    outfall_locs_url = 'https://docs.google.com/spreadsheets/d/1JJ8bPWppVKbmCfllIevrVmt_dcoswOim7Cos418Ot6w/edit?gid=0#gid=0'
)

BRC_locs <- locations_list$BRC
Outfall_locs <- locations_list$Outfall

accept_BRC <- acceptable_locs(BRC_locs)
accept_outfall <- acceptable_locs(Outfall_locs)

#################Now separate into the different data uploads##############################
####First Urban Riverfly data
BRC_UrbRiverfly <- clean_data(
    input_df = BRC_full_form,
    col_name_start = "data_type",
    col_name_end = "other_bullhead",
    sample_site = "sampling_site_riverfly",
    acceptable_site_orgs = accept_BRC,
    data_type_name = "Urban Riverfly"
)

# Water Quality data
BRC_WQ <- clean_data(
    input_df = BRC_full_form,
    col_name_start = "wq_sampling_site",
    col_name_end = "turbidity_NTU",
    sample_site = "wq_sampling_site",
    acceptable_site_orgs = accept_BRC,
    data_type_name = "Water Quality"
)

# Urban Outfall Safari data
BRCUrbOutSaf <- clean_data(
    input_df = BRC_full_form,
    col_name_start = "outfall_survey_date",
    col_name_end = "outfall_aesthetics",
    sample_site = "outfall_sampling_site",
    acceptable_site_orgs = accept_outfall,
    data_type_name = "Urban Outfall Safari"
)

# Invasive Species data
BRCInvSpcs <- clean_data(
    input_df = BRC_full_form,
    col_name_start = "invasive_spp_sampling_date",
    col_name_end = "any_other_invasive_spp",
    sample_site = "invasive_spp_sampling_site",
    acceptable_site_orgs = accept_BRC,
    data_type_name = "Invasive Species"
)

names(BRC_UrbRiverfly)[4] <- names(BRC_WQ)[3] <- names(BRC_locs)[
    2
] <- names(BRCInvSpcs)[4] <- "sampling_site"

# Add cleaned and processed data to package
usethis::use_data(BRC_UrbRiverfly, overwrite = TRUE)
usethis::use_data(BRC_WQ, overwrite = TRUE)
usethis::use_data(BRC_locs, overwrite = TRUE)
usethis::use_data(BRCInvSpcs, overwrite = TRUE)


# Create SQLite tables for riverfly, water quality, and associated location identifiers
# Invasive species and outfall safari data not currently being added to the database because they are empty
db_create("riverfly")
db_create("water_quality")
db_create("riverfly_locs")
db_create("invasive_species")

# Populate the database tables with the cleaned data
populate_db(BRC_UrbRiverfly, "riverfly")
populate_db(BRC_WQ, "water_quality")
populate_db(BRCInvSpcs, "invasive_species")
populate_db(BRC_locs, "riverfly_locs")

# usethis::use_data(data_20250922, overwrite = TRUE)
