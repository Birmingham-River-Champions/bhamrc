#' 01_welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_01_welcome_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "welcome-text",
      list_to_p(
        list(
          HTML(
            "<b> Birmingham River Champions (BRC) </b> is a citizen science project led by the University of Birmingham, partnered by the Environment Agency,
    Birmingham and Black Country Wildlife Trust and Severn Trent's river ranger team. We are connecting with volunteer groups 
    delivering <b> fantastic conservation </b> work across the West Midlands. BRC allows volunteers to monitor our vulnerable
    rivers by providing training and equipment. We want our <b> ‘river champions’ </b> to see the value in their results and aim to 
    communicate this back to the volunteers. We <b>value</b> the time of our river champion volunteers, so have picked <b>4 quick</b> techniques:
    Urban Riverfly, water chemistry monitoring (Temperature, Conductivity, Ammonia, Nitrate, Phosphate), Urban Outfall Safari 
    and invasive species spotting (see Information / Resources for further details). Urban Riverfly and water quality sampling
    should be repeated at the same site across the year, but Urban Outfall Safari and invasive species spotting can take place ‘out and about’
    when volunteers see relevant features.   We suggest groups commit about 1-2 hours monthly alongside their normal activities. Please do share this <b> <a href='www/BRC project overview.pdf' target='_blank'>project summary</a> </b> with volunteer groups or individuals
    that you think would be interested in getting involved."
          ),
          HTML(
            "Please email <b> birminghamriverchampions@gmail.com </b> if you have any questions 
    or would like more information on any aspects of the project"
          )
        ),
        class = "welcome-text"
      )
    )
  )
}

#' 01_welcome Server Functions
#'
#' @noRd
mod_01_welcome_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_01_welcome_ui("01_welcome_1")

## To be copied in the server
# mod_01_welcome_server("01_welcome_1")
