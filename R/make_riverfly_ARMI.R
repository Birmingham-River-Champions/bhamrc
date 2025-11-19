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
