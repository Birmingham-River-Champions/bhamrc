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
        tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = "brc_styles.css"
        )
      ),
      titlePanel(
        div(
          class = "title-panel",
          # Left side: larger logo.png image and text
          div(
            class = "logo-container",
            img(src = "www/logo.png", class = "logo"),
            div(
              class = "text-container",
              HTML(
                "<div>Monitor <span>your</span> river</div>
                <div>Protect <span>your</span> river</div>
                <div>Love <span>your</span> river</div>"
              )
            )
          ),
          # Right side: Smaller footer images, with EA logo slightly bigger
          div(
            img(src = "www/UoB logo.png", class = "UOB-logo"),
            img(src = "www/EA logo.png", class = "ea-logo"),
            img(src = "www/BBCWT logo.png", class = "footer-logo"),
            img(src = "www/Severn Trent logo.png", class = "ST-logo")
          )
        )
      ),
      tabsetPanel(
        tabPanel(
          "Project Overview",
          mod_01_welcome_ui("01_welcome_1"),
          div(
            class = "project-overview-images", # Grid for images
            div(
              class = 'img-container',
              img(src = 'www/Urban Riverfly group photo 1_6_24.jpg')
            ),
            div(
              class = 'img-container',
              img(
                src = 'www/Urban Riverfly training Rea Birmingham Uni volunteers.jpg'
              )
            ),
            div(
              class = 'img-container',
              img(src = 'www/JCW and Friends Trittiford.jpg')
            ),
            div(
              class = 'img-container',
              img(src = 'www/HGKIC with certificate.png')
            )
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
