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
              alt = "The logo of the Birmingham River Champions citizen science initiative"
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
              alt = "The logo of the University of Birmingham"
            ),
            img(
              src = "www/images/EA logo.png",
              class = "ea-logo",
              alt = "The logo of the Environment Agency"
            ),
            img(
              src = "www/images/BBCWT logo.png",
              class = "footer-logo",
              alt = "The logo of the Birmingham and Black Country Wildlife Trust"
            ),
            img(
              src = "www/images/Severn Trent logo.png",
              class = "ST-logo",
              alt = "The logo of Severn Trent Water"
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
                alt = "A photograph of volunteers at an Urban Riverfly training course"
              )
            ),
            div(
              class = 'img-container',
              img(
                src = 'www/images/Urban Riverfly training Rea Birmingham Uni volunteers.jpg',
                alt = "A photograph of an Urban Riverfly trainer demonstrating techniques to volunteers"
              )
            ),
            div(
              class = 'img-container',
              img(
                src = 'www/images/JCW and Friends Trittiford.jpg',
                alt = "A group of Birmingham River Champions volunteers collecting Urban Riverfly samples"
              )
            ),
            div(
              class = 'img-container',
              img(
                src = 'www/images/HGKIC with certificate.png',
                alt = "A photograph of volunteers holding their Urban Riverfly certification after receiving training"
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
          mod_06_newsletters_ui("06_newsletters_1")
        ),
        tabPanel(
          "Your Data",
          mod_03_plot_data_ui("03_plot_data_1")
        ),
        tabPanel(
          "Submitted Data",
          h3("Submitted Entries"),
          mod_05_show_data_ui("05_show_data_1"),
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
