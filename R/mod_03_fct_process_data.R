make_riverfly_ARMI <- function() {
  con <- dbi::dbConnect(RSQLite::SQLite(), "data.sqlite")
  riverfly_data <- DBI::dbReadTable(con, "riverfly")
  dbDisconnect(con)

  Riverfly_ARMI <-
    as.data.frame(sapply(riverfly_data, function(x) {
      x <- gsub("1-9", "1", x)
    }))
  #10-99 different between certain tolerant taxa and the rest
  Riverfly_ARMI <-
    Riverfly_ARMI |>
    dplyr::mutate(across(
      .cols = -matches("snails|leeches|worms|hoglouse"),
      .fns = ~ ifelse(. == '10-99', '2', .)
    )) |>
    dplyr::mutate(across(
      .cols = matches("snails|leeches|worms|hoglouse"),
      .fns = ~ ifelse(. == '10-99', '1', .)
    )) |>
    dplyr::mutate(across(
      .cols = -matches("snails|leeches|worms|hoglouse|blackfly"),
      .fns = ~ ifelse(. == '100-999', '3', .)
    )) |>
    dplyr::mutate(across(
      .cols = matches("blackfly"),
      .fns = ~ ifelse(. == '100-999', '2', .)
    )) |>
    dplyr::mutate(across(
      .cols = matches("snails"),
      .fns = ~ ifelse(. == '100-999', '1', .)
    )) |>
    dplyr::mutate(across(
      .cols = matches("leeches|worms|hoglouse"),
      .fns = ~ ifelse(. == '100-999', '0', .)
    )) |>
    dplyr::mutate(across(
      .cols = -matches("snails|leeches|worms|hoglouse|blackfly|shrimp"),
      .fns = ~ ifelse(. == '>1000', '4', .)
    )) |>
    dplyr::mutate(across(
      .cols = matches("shrimp"),
      .fns = ~ ifelse(. == '>1000', '2', .)
    )) |>
    dplyr::mutate(across(
      .cols = matches("blackfly|snails"),
      .fns = ~ ifelse(. == '>1000', '0', .)
    )) |>
    dplyr::mutate(across(
      .cols = matches("leeches|hoglouse"),
      .fns = ~ ifelse(. == '>1000', '-2', .)
    )) |>
    dplyr::mutate(across(
      .cols = matches("worms"),
      .fns = ~ ifelse(. == '>1000', '-3', .)
    )) |>
    mutate_at(vars(matches("Number of")), as.numeric)

  Riverfly_ARMI_Calc <-
    as.data.frame(
      Riverfly_ARMI |>
        rowwise() |>
        mutate(
          ARMI = sum(
            c_across(
              matches("Number of")
            ),
            na.rm = TRUE
          )
        ) |> #,
        ##Ntaxa = sum(c_across(matches("Number of")) > 0, na.rm = TRUE))|> THOUGHT IT WAS CALCULATED LIKE ASPT, BUT IS ACTUALLY LIKE BMWP
        dplyr::select(-contains("Number of"))
    )
}

make_ARMI_plot_data <- function(Riverfly_ARMI_Calc, Unique_BRC_Sampling_Locs) {
  ##Now bring this in
  Riverfly_ARMI_Plot <-
    left_join(
      Riverfly_ARMI_Calc,
      Unique_BRC_Sampling_Locs,
      by = c("BRC sampling site ID...5" = "BRC sampling site ID")
    )
  ##And colour code the point according to the ARMI score
  Riverfly_ARMI_Plot <- Riverfly_ARMI_Plot %>%
    mutate(
      ARMI_Plot_Colour = cut(
        ARMI,
        breaks = c(-Inf, 5:14, Inf),
        labels = brewer.pal(n = 11, name = "RdBu")
      )
    ) %>% #11=max no. colours, xtreme red to xtreme blue- Blue rather than green so its colorblind friendly
    dplyr::select(Organisation, everything())

  #######Remove the parenthsised organisation from the site ID
  Riverfly_ARMI_Plot <- Riverfly_ARMI_Plot %>%
    dplyr::rename(`BRC site ID` = "BRC sampling site ID...5")
  Riverfly_ARMI_Plot$`BRC site ID` <- gsub(
    "\\s*\\(.*\\)$",
    "",
    Riverfly_ARMI_Plot$`BRC site ID`
  )
  ##Now also flip the names around so the RIVER comes after the Site
  flip_site_names <- function(site_name) {
    # Use regex to capture the two parts of the string
    gsub("^(\\w+(?: \\w+)*),\\s*(.*)$", "\\2, \\1", site_name)
  }
  # Code was from ChatGPT initially designed for multiple columns, but works fine.
  Riverfly_ARMI_Plot <- Riverfly_ARMI_Plot %>%
    mutate(across(c("BRC site ID"), flip_site_names))

  #dropping unwanted columns and arrange by date
  Riverfly_ARMI_Plot <- Riverfly_ARMI_Plot %>%
    dplyr::select(-c(Easting, Northing)) %>%
    mutate(`Survey date` = dmy(`Survey date`)) %>%
    arrange(`BRC site ID`, `Survey date`)

  ###Anonymise those that want to be based on the sign up sheet
  Riverfly_ARMI_Plot$Organisation <- ifelse(
    Riverfly_ARMI_Plot$Organisation == "Friends of Lifford Reservoir",
    "Anonymous",
    Riverfly_ARMI_Plot$Organisation
  )

  ##Now get average value for plotting purposes - in time I want to only select the last 12 months
  Riverfly_ARMI_Plot_SiteAv <- Riverfly_ARMI_Plot[, c(
    "BRC site ID",
    "ARMI",
    "Organisation"
  )] %>%
    group_by(`BRC site ID`, Organisation) %>%
    summarise_all(mean) %>%
    mutate(
      ARMI_Plot_Colour = cut(
        ARMI,
        breaks = c(-Inf, c(5:14), Inf),
        labels = c(brewer.pal(n = 11, name = "RdBu"))
      )
    ) %>%
    ungroup() #11=max no. colours, xtreme red to xtreme blue
  ##
  Riverfly_ARMI_Plot_SiteAv <- left_join(
    Riverfly_ARMI_Plot_SiteAv,
    unique(Riverfly_ARMI_Plot[, c("BRC site ID", "LAT", "LONG")]),
    by = "BRC site ID"
  )

  return(Riverfly_ARMI_Plot_SiteAv)
}
