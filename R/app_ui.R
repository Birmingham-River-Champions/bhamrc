#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
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
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "bhamrc"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
