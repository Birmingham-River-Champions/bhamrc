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
        stonefly_plecoptera = c(
            "10-99",
            "100-999",
            "100-999",
            "",
            ">1000",
            "10-99"
        )
    )

    sql_string <- "CREATE TABLE riverflytest (id INTEGER PRIMARY KEY, organisation TEXT, survey_date TEXT, data_type TEXT,
            sampling_site TEXT, cased_caddisfly INTEGER, stonefly_plecoptera INTEGER)"

    con <- dbConnect(RSQLite::SQLite(), "data.sqlite")
    if (!dbExistsTable(con, "riverflytest")) {
        dbExecute(con, sql_string)
    } else {
        message(paste(
            "Table riverflytest already exists. Deleting and recreating."
        ))
        dbRemoveTable(con, "riverflytest")
        dbExecute(con, sql_string)
    }

    dbDisconnect(con)

    #Use the test fixture to populate a test DB
    populate_db(test_df, "riverflytest")

    test_locs_df <- data.frame(
        organisation = c("Org1", "Org1", "Org2", "Org2", "Org3", "Org3"),

        ID = c("SiteA", "SiteA", "SiteB", "SiteB", "SiteD", "SiteD"),
        LAT = c(52.1, 52.1, 52.2, 52.2, 52.3, 52.3),
        LONG = c(-1.87, -1.87, -1.97, -1.97, -2.007, -2.007)
    )

    return(list("test_data" = test_df, "sampling_locs" = test_locs_df))
}
