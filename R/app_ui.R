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
        HTML("<html lang='en'>"),
        # Css stylesheet
        tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = "custom.css"
        )
      ),
      titlePanel(
        div(
          class = "title-panel",
          # Left side: larger logo.png image and text
          div(
            class = "logo-container",
            img(
              src = "www/images/logo.png",
              class = "logo",
              alt = "Birmingham River Champions logo"
            ),
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
            img(
              src = "www/images/UoB logo.png",
              class = "UOB-logo",
              alt = "University of Birmingham logo"
            ),
            img(
              src = "www/images/EA logo.png",
              class = "ea-logo",
              alt = "Environment Agency logo"
            ),
            img(
              src = "www/images/BBCWT logo.png",
              class = "footer-logo",
              alt = "Birmingham and Black Country Wildlife Trust logo"
            ),
            img(
              src = "www/images/Severn Trent logo.png",
              class = "ST-logo",
              alt = "Severn Trent logo"
            )
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
              img(
                src = 'www/images/Urban Riverfly group photo 1_6_24.jpg',
                alt = "Riverfly survey group photo"
              )
            ),
            div(
              class = 'img-container',
              img(
                src = 'www/images/Urban Riverfly training Rea Birmingham Uni volunteers.jpg',
                alt = "Riverfly training session with Birmingham University volunteers"
              )
            ),
            div(
              class = 'img-container',
              img(
                src = 'www/images/JCW and Friends Trittiford.jpg',
                alt = "JCW and Friends at Trittiford"
              )
            ),
            div(
              class = 'img-container',
              img(
                src = 'www/images/HGKIC with certificate.png',
                alt = "Hall Green Keepin' It Clean with certificate"
              )
            )
          )
        ),
        tabPanel(
          "Information / resources",
          mod_04_information_ui("04_information_1")
        ),
        tabPanel(
          "Newsletters / reports",
          p("Share newsletters and updates here.")
        ),
        tabPanel(
          "Your Data",
          mod_03_plot_data_ui("03_plot_data_1")
        ),
        tabPanel(
          "Submitted Data",
          h3("Submitted Entries"),
          DT::DTOutput("entries")
        ),
        tabPanel(
          "Submission Form",
          mod_02_data_input_ui("02_data_input_1")
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
      app_title = "Birmingham River Champions"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
