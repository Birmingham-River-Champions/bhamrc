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
