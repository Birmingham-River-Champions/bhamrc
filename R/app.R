library(shiny)
library(DBI)
library(RSQLite)

# Database setup
con <- dbConnect(RSQLite::SQLite(), "data.sqlite")
if (!dbExistsTable(con, "submissions")) {
    dbExecute(
        con,
        "CREATE TABLE submissions (id INTEGER PRIMARY KEY, name TEXT, email TEXT, comment TEXT)"
    )
}

ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    titlePanel("Birmingham River Champions"),
    tabsetPanel(
        tabPanel(
            "Project Overview",
            h3("Project Overview"),
            p(
                "This tab can be customized to show project details, images, and more."
            )
        ),
        tabPanel(
            "Information",
            h3("Information"),
            p("Add information and resources here.")
        ),
        tabPanel(
            "Newsletters",
            h3("Newsletters"),
            p("Share newsletters and updates here.")
        ),
        tabPanel(
            "Your Data",
            h3("Submitted Entries"),
            DT::DTOutput("entries")
        ),
        tabPanel(
            "Submission Form",
            sidebarLayout(
                sidebarPanel(
                    textInput("name", "Name"),
                    textInput("email", "Email"),
                    textAreaInput("comment", "Comment"),
                    actionButton("submit", "Submit", class = "btn-primary")
                ),
                mainPanel(
                    h3("Submit your entry using the form.")
                )
            )
        )
    )
)

server <- function(input, output, session) {
    observeEvent(input$submit, {
        req(input$name, input$email, input$comment)
        dbExecute(
            con,
            "INSERT INTO submissions (name, email, comment) VALUES (?, ?, ?)",
            params = list(input$name, input$email, input$comment)
        )
        showNotification("Submission successful!", type = "message")
    })

    output$entries <- DT::renderDT({
        dbGetQuery(
            con,
            "SELECT name, email, comment FROM submissions ORDER BY id DESC"
        )
    })
}

onStop(function() {
    dbDisconnect(con)
})

shinyApp(ui, server)
