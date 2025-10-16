test_values <- test_fixture_riverfly()
test_df <- test_values[[1]]

locs_name <- "BRC_Sampling_Locs"
test_locs <- read.csv(test_path("../../inst/extdata/BRC_Sampling_Locs.csv"))
acceptable_site_orgs <- acceptable_locs(test_locs)

plot_test_data <- left_join(
  test_df,
  test_locs[, c(
    "ID",
    "LAT",
    "LONG"
  )],
  by = c("sampling_site" = "ID"),
  multiple = "first"
)

other_spp_plot_test <- select(
  plot_test_data,
  survey_date,
  sampling_site,
  organisation,
  LAT,
  LONG,
  one_of(names(other_spp_bw)),
) |>
  pivot_longer(
    -c(
      organisation,
      survey_date,
      sampling_site,
      LAT,
      LONG
    )
  ) |>
  rename(taxa = "name", abundance = "value") |>
  group_by(sampling_site, taxa) |>
  filter(!all(abundance == "0")) |>
  ungroup() |>
  mutate(
    abundance = factor(
      abundance,
      levels = c(">1000", "100-999", "10-99", "1-9")
    )
  ) |>
  mutate(survey_date = dmy(survey_date))

other_spp_plot_test$organisation <- ifelse(
  other_spp_plot_test$organisation == "Friends of Lifford Reservoir",
  "Anonymous",
  other_spp_plot_test$organisation
)

other_spp_plot_test$sampling_site <- gsub(
  "\\s*\\(.*\\)$",
  "",
  other_spp_plot_test$sampling_site
)
other_spp_plot_test <- other_spp_plot_test |>
  mutate(across(sampling_site, flip_site_names))

riverfly_species_plot_test <- select(
  plot_test_data,
  survey_date,
  sampling_site,
  organisation,
  one_of(names(riverfly_spp_bw)),
  LAT,
  LONG
) |>
  pivot_longer(
    -c(
      organisation,
      survey_date,
      sampling_site,
      LAT,
      LONG
    )
  ) |>
  rename(taxa = "name", abundance = "value") |>
  group_by(sampling_site, taxa) |>
  filter(!all(abundance == "0")) |>
  ungroup() |>
  mutate(
    abundance = as.numeric(case_when(
      abundance == "0" ~ 0,
      abundance == "1-9" ~ 1,
      abundance == "10-99" ~ 2,
      abundance == "100-999" ~ 3,
      abundance == ">1000" ~ 4
    ))
  ) |>
  mutate(survey_date = dmy(survey_date))

riverfly_species_plot_test$organisation <- ifelse(
  riverfly_species_plot_test$organisation == "Friends of Lifford Reservoir",
  "Anonymous",
  riverfly_species_plot_test$organisation
)
riverfly_species_plot_test$sampling_site <- gsub(
  "\\s*\\(.*\\)$",
  "",
  riverfly_species_plot_test$sampling_site
)
riverfly_species_plot_test <- riverfly_species_plot_test |>
  mutate(across(sampling_site, flip_site_names))

riverfly_plot_test_recent <- riverfly_species_plot_test |>
  filter(survey_date >= Sys.Date() - years(3)) |>
  arrange(sampling_site, taxa, desc(abundance), desc(survey_date)) |>
  group_by(sampling_site, taxa) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(
    Riverfly_Species_Colour = factor(
      case_when(
        abundance == 0 ~ brewer.pal(n = 5, name = "Greys")[1],
        abundance == 1 ~ brewer.pal(n = 5, name = "Greys")[2],
        abundance == 2 ~ brewer.pal(n = 5, name = "Greys")[3],
        abundance == 3 ~ brewer.pal(n = 5, name = "Greys")[4],
        abundance == 4 ~ brewer.pal(n = 5, name = "Greys")[5]
      ),
      levels = brewer.pal(n = 5, name = "Greys")
    )
  )

other_plot_test_recent <- other_spp_plot_test |>
  filter(survey_date >= Sys.Date() - years(3)) |>
  arrange(sampling_site, taxa, abundance, desc(survey_date)) |>
  group_by(sampling_site, taxa) |>
  slice_head(n = 1) |>
  ungroup()

test_plot <- list(
  riverfly_species_plot_test,
  riverfly_plot_test_recent,
  other_spp_plot_test,
  other_plot_test_recent
)

test_spp <- species_plots(test_df, test_locs)

test_that("full riverfly species_plots work", {
  expect_equal(test_spp[[1]], test_plot[[1]])
})

test_that("make_recent_riverfly works", {
  expect_equal(test_spp[[2]], test_plot[[2]])
})

test_that("make_other_spp_plot works", {
  expect_equal(test_spp[[3]], test_plot[[3]])
})

test_that("make_recent_other_spp works", {
  expect_equal(test_spp[[4]], test_plot[[4]])
})

test_that("make_recent_inv_spp works", {
  plot_palette <- brewer.pal(n = 9, name = "PuBu")
  inv_spp_test <- left_join(
    test_values[[4]],
    test_locs[, c(
      "ID",
      "LAT",
      "LONG"
    )],
    by = c("sampling_site" = "ID"),
  ) |>
    dplyr::select(
      -id,
      -invasive_spp_what_three_words,
      -any_other_invasive_spp,
      -data_type
    ) |>
    pivot_longer(
      -c(
        organisation,
        survey_date,
        invasive_spp_sampling_date,
        sampling_site,
        LAT,
        LONG
      ),
      names_to = "variable",
      values_to = "value"
    ) |>
    group_by(sampling_site, variable) |>
    dplyr::filter(value != "") |>
    dplyr::rename(c(
      'measurement' = variable,
      'abundance' = value
    )) |>
    mutate(
      InvSpcs_Plot_Colour = dplyr::case_when(
        abundance == ">1000" ~ plot_palette[1],
        abundance == "100-999" ~ plot_palette[2],
        abundance == "10-99" ~ plot_palette[3],
        abundance == "1-9" ~ plot_palette[4],
        abundance == "Abundant (>33%)" ~ plot_palette[1],
        abundance == "Present (1-33%)" ~ plot_palette[3]
      )
    ) |>
    ungroup() |>
    rename('date_time' = invasive_spp_sampling_date) |>
    dplyr::arrange(sampling_site, date_time) |>
    dplyr::select(
      -survey_date
    ) |>
    dplyr::mutate(date_time = as.Date(lubridate::dmy(date_time))) |>
    dplyr::mutate(sampling_site = gsub("\\s+", " ", sampling_site)) |> # Remove the parenthsised organisation from the site ID
    mutate(across(sampling_site, flip_site_names))

  inv_spp_test_recent <- inv_spp_test |>
    filter(date_time >= Sys.Date() - years(3))

  inv_spp_test_recent$abundance <- factor(
    inv_spp_test_recent$abundance,
    levels = c(
      ">1000",
      "100-999",
      "10-99",
      "1-9",
      "Abundant (>33%)",
      "Present (1-33%)"
    )
  )

  inv_spp_test_recent <- inv_spp_test_recent |>
    dplyr::arrange(
      sampling_site,
      measurement,
      abundance,
      desc(date_time)
    ) |>
    group_by(sampling_site, measurement) |>
    slice_head(n = 1) |>
    ungroup() |>
    dplyr::relocate(date_time, .after = InvSpcs_Plot_Colour)

  inv_spp_locs <- test_locs |>
    rename('sampling_site' = ID)
  test_inv_spp <- make_recent_inv_spp(
    test_values[[4]],
    inv_spp_locs,
    plot_palette
  )

  expect_equal(test_inv_spp, inv_spp_test_recent)
})

test_that("flip_site_names works", {
  site_names <- c(
    "River, Tame",
    "Canal, Birmingham",
    "Lake, Edgbaston",
    "SiteA",
    "SiteB"
  )

  flip_test <- c(
    "Tame, River",
    "Birmingham, Canal",
    "Edgbaston, Lake",
    "SiteA",
    "SiteB"
  )

  flipped <- flip_site_names(site_names)

  expect_equal(flip_test, flipped)
})
