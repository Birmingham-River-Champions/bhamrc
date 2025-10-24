#' Sum up ARMI observations across taxa
#' @param Riverfly_ARMI A data frame containing Riverfly ARMI observations per taxa
#' @return A data frame with ARMI summed across taxa
#' @importFrom dplyr rowwise mutate select all_of
#' @importFrom dplyr c_across
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
