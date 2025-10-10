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
        )
    )

    con <- dbConnect(RSQLite::SQLite(), "data.sqlite")
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

    return(list("test_data" = test_df, "sampling_locs" = test_locs_df))
}
