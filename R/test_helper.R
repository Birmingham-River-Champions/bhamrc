#' @noRd
test_fixture_riverfly <- function() {
    # Create a test fixture for the data
    test_df <- data.frame(
        organisation = c("Org1", "Org1", "Org2", "Org2", "Org3", "Org3"),
        data_type = rep("TestType", 6),
        survey_date = as.Date(c(
            "2023-10-01",
            "2023-10-05",
            "2023-10-02",
            "2023-10-03",
            "2023-10-04",
            "2023-10-06"
        )),
        sampling_site = c("SiteA", "SiteA", "SiteB", "SiteB", "SiteD", "SiteD"),
        cased_caddisfly = c("10-99", "100-999", "1-9", "", ">1000", "10-99"),
        caseless_caddisfly = c("1-9", "10-99", "100-999", "10-99", "", ">1000"),
        olive_mayfly = c("10-99", "100-999", "1-9", "100-999", "10-99", ""),
        blue_winged_olive_mayfly = c(
            "100-999",
            "10-99",
            "",
            "1-9",
            "10-99",
            ">1000"
        ),
        freshwater_shrimp = c(
            "10-99",
            "",
            "10-99",
            "1-9",
            "100-999",
            "100-999"
        ),
        freshwater_hoglouse = c(
            "1-9",
            "100-999",
            "10-99",
            "100-999",
            "",
            "10-99"
        ),
        blackfly_larvae = c("10-99", "1-9", "100-999", "10-99", "100-999", ""),
        freshwater_worm = c("100-999", "10-99", "1-9", "100-999", "", "10-99"),
        freshwater_leech = c("10-99", "100-999", "10-99", "1-9", "100-999", ""),
        freshwater_snail = c("1-9", "10-99", "100-999", "", "10-99", ">1000"),
        freshwater_beetle = c(
            "100-999",
            "1-9",
            "10-99",
            "100-999",
            "10-99",
            ""
        ),
        green_drake_mayfly = c(
            "10-99",
            "100-999",
            "1-9",
            "",
            "100-999",
            "10-99"
        ),
        flat_bodied_stone_clinger_mayfly = c(
            "1-9",
            "10-99",
            "100-999",
            "10-99",
            ">1000",
            ""
        ),
        stonefly_plecoptera = c(
            "1-9",
            "10-99",
            "100-999",
            "10-99",
            ">1000",
            ""
        ),
        other_chironomidae = c("10-99", "1-9", "100-999", "10-99", "", "10-99"),
        other_dicranota = c("1-9", "10-99", "100-999", "", "10-99", ">1000"),
        other_tipulidae = c("100-999", "1-9", "10-99", "100-999", "10-99", ""),
        other_hydracarina = c(
            "10-99",
            "100-999",
            "10-99",
            "1-9",
            "100-999",
            ""
        ),
        other_hydropsychidae = c(
            "1-9",
            "10-99",
            "100-999",
            "10-99",
            ">1000",
            ""
        ),
        other_rhyacophilidae = c(
            "1-9",
            "10-99",
            "100-999",
            "10-99",
            ">1000",
            ""
        ),
        other_planorbidae = c("10-99", "1-9", "100-999", "10-99", "", "10-99"),
        other_sphaeriidae = c("1-9", "10-99", "100-999", "", "10-99", ">1000"),
        other_acroloxidae_ancylidae = c(
            "100-999",
            "1-9",
            "10-99",
            "100-999",
            "10-99",
            ""
        ),
        other_bullhead = c("10-99", "100-999", "10-99", "1-9", "100-999", "")
    )

    con <- dbConnect(RSQLite::SQLite(), "data.sqlite", extended_types = TRUE)
    if (!dbExistsTable(con, "riverflytest")) {
        dbWriteTable(con, "riverflytest", test_df)
    } else {
        message(paste(
            "Table riverflytest already exists. Deleting and recreating."
        ))
        dbRemoveTable(con, "riverflytest")
        dbWriteTable(con, "riverflytest", test_df)
    }

    dbDisconnect(con)

    test_locs_df <- data.frame(
        organisation = c("Org1", "Org1", "Org2", "Org2", "Org3", "Org3"),

        ID = c("SiteA", "SiteA", "SiteB", "SiteB", "SiteD", "SiteD"),
        LAT = c(52.1, 52.1, 52.2, 52.2, 52.3, 52.3),
        LONG = c(-1.87, -1.87, -1.97, -1.97, -2.007, -2.007)
    )

    inv_spp_test <- data.frame(
        organisation = c("Org1", "Org1", "Org2", "Org2", "Org3", "Org3"),
        survey_date = as.Date(c(
            "2023-10-01",
            "2023-10-05",
            "2023-10-02",
            "2023-10-03",
            "2023-10-04",
            "2023-10-06"
        )),
        invasive_spp_sampling_date = as.Date(c(
            "2023-10-01",
            "2023-10-05",
            "2023-10-02",
            "2023-10-03",
            "2023-10-04",
            "2023-10-06"
        )),
        invasive_spp_what_three_words = rep("", 6),
        any_other_invasive_spp = rep("", 6),
        sampling_site = c("SiteA", "SiteA", "SiteB", "SiteB", "SiteD", "SiteD"),
        signal_crayfish = c("1-9", "10-99", "100-999", "10-99", ">1000", "1-9"),
        killer_shrimp = c("10-99", "1-9", "100-999", "10-99", ">1000", ""),
        himalayan_balsam = c(
            "Present (1-33%)",
            "Present (1-33%)",
            "Present (1-33%)",
            NA,
            "Present (1-33%)",
            "Abundant (>33%)"
        ),
        japanese_knotweed = c(
            "Present (1-33%)",
            "Abundant (>33%)",
            "Present (1-33%)",
            "Abundant (>33%)",
            NA,
            "Present (1-33%)"
        ),
        giant_hogweed = c(
            "Present (1-33%)",
            "Present (1-33%)",
            "Abundant (>33%)",
            "Present (1-33%)",
            "Abundant (>33%)",
            NA
        )
    )

    return(list(
        "test_data" = test_df,
        "sampling_locs" = test_locs_df,
        "invasive_spp" = inv_spp_test
    ))
}
