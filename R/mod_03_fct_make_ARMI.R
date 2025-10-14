#' make_riverfly_ARMI
#'
#' @description A function to add the ARMI values to the riverfly data
#' @param riverfly_data - riverfly data frame from
#' @return The data frame with riverfly data and an appended ARMI column.
#' @importFrom DBI dbReadTable dbConnect dbDisconnect
#' @importFrom dplyr mutate mutate_at left_join select vars c_across
#' @importFrom dplyr summarise group_by join_by
make_riverfly_ARMI <- function(riverfly_data) {
  riverfly_data <-
    as.data.frame(sapply(riverfly_data, function(x) {
      x <- gsub("1-9", "1", x)
    }))

  # 10-99 different between certain tolerant taxa and the rest
  # TODO: change this to case_when for clarity
  Riverfly_ARMI <-
    riverfly_data |>
    dplyr::mutate(across(
      .cols = -matches("snail|leech|worm|hoglouse"),
      .fns = ~ ifelse(. == '10-99', '2', .)
    )) |>
    dplyr::mutate(across(
      .cols = matches("snail|leech|worm|hoglouse"),
      .fns = ~ ifelse(. == '10-99', '1', .)
    )) |>
    dplyr::mutate(across(
      .cols = -matches("snail|leech|worm|hoglouse|blackfly"),
      .fns = ~ ifelse(. == '100-999', '3', .)
    )) |>
    dplyr::mutate(across(
      .cols = matches("blackfly"),
      .fns = ~ ifelse(. == '100-999', '2', .)
    )) |>
    dplyr::mutate(across(
      .cols = matches("snail"),
      .fns = ~ ifelse(. == '100-999', '1', .)
    )) |>
    dplyr::mutate(across(
      .cols = matches("leech|worm|hoglouse"),
      .fns = ~ ifelse(. == '100-999', '0', .)
    )) |>
    dplyr::mutate(across(
      .cols = -matches("snail|leech|worm|hoglouse|blackfly|shrimp"),
      .fns = ~ ifelse(. == '>1000', '4', .)
    )) |>
    dplyr::mutate(across(
      .cols = matches("shrimp"),
      .fns = ~ ifelse(. == '>1000', '2', .)
    )) |>
    dplyr::mutate(across(
      .cols = matches("blackfly|snail"),
      .fns = ~ ifelse(. == '>1000', '0', .)
    )) |>
    dplyr::mutate(across(
      .cols = matches("leech|hoglouse"),
      .fns = ~ ifelse(. == '>1000', '-2', .)
    )) |>
    dplyr::mutate(across(
      .cols = matches("worm"),
      .fns = ~ ifelse(. == '>1000', '-3', .)
    )) |>
    mutate(across(all_of(names(riverfly_spp_bw)), as.numeric))

  return(Riverfly_ARMI)
}

sum_up_ARMI <- function(Riverfly_ARMI) {
  # Sum ARMI observations across taxa and remove individual species observations
  Riverfly_ARMI_Calc <-
    as.data.frame(
      Riverfly_ARMI |>
        rowwise() |>
        mutate(
          ARMI = sum(
            c_across(all_of(names(riverfly_spp_bw))),
            na.rm = TRUE
          )
        ) |> #,
        ##Ntaxa = sum(c_across(matches("Number of")) > 0, na.rm = TRUE))|> THOUGHT IT WAS CALCULATED LIKE ASPT, BUT IS ACTUALLY LIKE BMWP
        dplyr::select(-all_of(names(riverfly_spp_bw)))
    ) # Should replace this with name constant eventually)
  return(Riverfly_ARMI_Calc)
}

#' make_ARMI_plot_data
#'
#' @description A function to make the plot object for the ARMI map and tooltip graphs
#' @param Riverfly_ARMI_Calc A dataframe output from make_riverfly_ARMI that contains
#' riverfly data and the ARMI column
#' @param Unique_BRC_Sampling_Locs a data frame that contains the geographic coordinates
#' for the sampling sites.
#' @return averaged site ARMI calculations to plot on the riverfly figure
#' @importFrom dplyr left_join mutate summarise_all
make_ARMI_plot_data <- function(Riverfly_ARMI_Calc, Unique_BRC_Sampling_Locs) {
  ##Now bring this in
  Riverfly_ARMI_Plot <-
    left_join(
      Riverfly_ARMI_Calc,
      Unique_BRC_Sampling_Locs,
      by = c("sampling_site" = "ID")
    )

  ##And colour code the point according to the ARMI score
  Riverfly_ARMI_Plot <- Riverfly_ARMI_Plot |>
    mutate(
      ARMI_Plot_Colour = cut(
        ARMI,
        breaks = c(-Inf, 5:14, Inf),
        labels = brewer.pal(n = 11, name = "RdBu")
      )
    ) |> #11=max no. colours, xtreme red to xtreme blue- Blue rather than green so its colorblind friendly
    dplyr::select(organisation, everything())

  #######Remove the parenthsised organisation from the site ID
  Riverfly_ARMI_Plot <- Riverfly_ARMI_Plot |>
    mutate(gsub(
      "\\s*\\(.*\\)$",
      "",
      sampling_site
    ))
  ##Now also flip the names around so the RIVER comes after the Site

  # Code was from ChatGPT initially designed for multiple columns, but works fine.
  Riverfly_ARMI_Plot <- Riverfly_ARMI_Plot |>
    mutate(across(c(sampling_site), flip_site_names)) |>
    dplyr::select(-c(Easting, Northing)) |>
    mutate(survey_date = dmy(survey_date)) |>
    arrange(sampling_site, survey_date)

  # Make organisation anonymous if desired
  Riverfly_ARMI_Plot$organisation <- ifelse(
    Riverfly_ARMI_Plot$organisation == "Friends of Lifford Reservoir",
    "Anonymous",
    Riverfly_ARMI_Plot$organisation
  )

  ##Now get average value for plotting purposes - in time I want to only select the last 12 months
  Riverfly_ARMI_Plot_SiteAv <- Riverfly_ARMI_Plot |>
    select(sampling_site, ARMI, organisation) |>
    group_by(sampling_site, organisation) |>
    summarise_all(mean) |>
    mutate(
      ARMI_Plot_Colour = cut(
        ARMI,
        breaks = c(-Inf, c(5:14), Inf),
        labels = c(brewer.pal(n = 11, name = "RdBu"))
      )
    ) |>
    ungroup() #11=max no. colours, xtreme red to xtreme blue
  ##

  Riverfly_ARMI_Plot_SiteAv <- left_join(
    Riverfly_ARMI_Plot_SiteAv,
    unique(Riverfly_ARMI_Plot[, c("sampling_site", "LAT", "LONG")]),
    multiple = "first"
  )

  return(list(Riverfly_ARMI_Plot, Riverfly_ARMI_Plot_SiteAv))
}
