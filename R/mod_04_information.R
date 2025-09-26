#' 04_information UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_04_information_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Main Title (centered)
    h1(id = "main-title", HTML("<b>Key information</b>")), # Using HTML() for bold text

    # Grid layout for left (summary) and right (news post)
    div(
      id = "project-grid",

      # First row: Left column (Image + Text)
      div(
        class = "grid-item",
        img(src = 'Person clipboard.jpg'), # Image
        div(
          class = "text-container",
          h2(a(
            href = "https://docs.google.com/forms/d/e/1FAIpQLSeqf1PdH06bv5J9q-DqdJ2A8KqAg8cvtOH7wWiux7p1vKc6Gw/viewform?usp=sf_link",
            target = "_blank",
            'Survey form'
          )),
          h4(HTML(
            'Follow this link to upload data on all <b>four</b> techniques used in the Birmingham River Champions'
          ))
        )
      ),

      # First row: Right column (Image + Text)
      div(
        class = "grid-item",
        img(src = 'Friends Trittiford sampling.jpg'), # New Image
        div(
          class = "text-container",
          h2(a(
            href = "BRC volunteer risk assessment.pdf",
            target = "_blank",
            'Risk assessment'
          )),
          h4(HTML(
            'Information on key risks that volunteers can experience when sampling and how to mitigate these.'
          ))
        )
      ),
      # Second row: New left column (Image + Text)
      div(
        class = "grid-item",
        style = "margin-bottom: 20px;", # Add space under this row
        img(src = 'Hands up.png'), # New Image
        div(
          class = "text-container",
          h2(a(
            href = "Volunteer group lead sign up info.pdf",
            target = "_blank",
            'Register a new group'
          )),
          h4(HTML(
            'Information for volunteer group leads on how to get their organisation involved - please read this and then click <a href="https://forms.office.com/Pages/ResponsePage.aspx?id=z8oksN7eQUKhXDyX1VPp81sI4vhUl6NFmBOJwbJoQY1UQUZPVVU2NzFHT0FRN0FIQzYwSzc5VkszUy4u" target="_blank">here</a> to register'
          ))
        )
      ),
      # Second row: New right column (Image + Text)
      div(
        class = "grid-item",
        img(src = 'Hands up.png'), # New Image
        div(
          class = "text-container",
          style = "margin-bottom: 20px;", # Add space under this row
          h2(a(
            href = "Individual volunteer sign up info.pdf",
            target = "_blank",
            'Volunteer sign up information'
          )),
          h4(HTML(
            'Information for volunteers of a registered group who want to get involved - please read this and then click <a href="https://forms.office.com/Pages/ResponsePage.aspx?id=z8oksN7eQUKhXDyX1VPp81sI4vhUl6NFmBOJwbJoQY1UMlpNWVdLVkRLU0JLR0U3RDhTVk1FTjU4Si4u" target="_blank">here</a>'
          ))
        )
      )
    ),
    #REPEAT FOR URBAN RIVERFLY
    # h1(id = "main-title", HTML("<b>Urban riverfly</b>")),  # Using HTML() for bold text

    # Grid layout for left (summary) and right (news post)
    div(
      id = "project-grid",

      # First row: Left column (Image + Text)
      div(
        class = "grid-item",
        style = "margin-bottom: 20px;", # Add space under this row
        img(src = 'JCW kick sampling.png'), # Image
        div(
          class = "text-container",
          h2(a(
            href = "BRC Urban Riverfly guidance.pdf",
            target = "_blank",
            'Urban Riverfly guidance'
          )),
          h4(HTML(
            'Follow this link to see information on how to effectively undertake kick sampling and macroinvertebrate identification'
          ))
        )
      ),

      # First row: Right column (Image + Text)
      div(
        class = "grid-item",
        style = "margin-bottom: 20px;", # Add space under this row
        img(src = 'Person clipboard.jpg'), # New Image
        div(
          class = "text-container",
          h2(a(
            href = "BRC Urban Riverfly Print Out Survey Form.pdf",
            target = "_blank",
            'Paper copy of the Urban Riverfly survey form'
          )),
          h4(HTML(
            'Click this link to see a paper version of the Urban Riverfly survey form, which you can complete in the field before submitting the results online via the link above'
          ))
        )
      )
    ),
    #REPEAT FOR WATER CHEMISTRY
    #h1(id = "main-title", HTML("<b>Water chemistry</b>")),  # Using HTML() for bold text

    # Grid layout for left (summary) and right (news post)
    div(
      id = "project-grid",

      # First row: Left column (Image + Text)
      div(
        class = "grid-item",
        style = "margin-bottom: 20px;", # Add space under this row
        img(src = 'Kyoritsu volunteer blurred picture.jpg'), # Image
        div(
          class = "text-container",
          h2(a(
            href = "BRC water chemistry guidance.pdf",
            target = "_blank",
            'Water chemistry guidance'
          )),
          h4(HTML(
            'Follow this link to see guidance on how to effectively undertake water chemistry samples, detecting a \'cocktail\' of different pollutants
                      via various pieces of equipment'
          ))
        )
      ),

      # First row: Right column (Image + Text)
      div(
        class = "grid-item",
        style = "margin-bottom: 20px;", # Add space under this row
        img(src = 'Person clipboard.jpg'), # New Image
        div(
          class = "text-container",
          h2(a(
            href = "BRC Water Chemistry Print Out Survey Form.pdf",
            target = "_blank",
            'Paper copy of the water chemistry survey form'
          )),
          h4(HTML(
            'Click this link to see a paper version of the water chemistry survey form, which you can complete in the field before submitting the results online via the link above'
          ))
        )
      )
    ),
    #REPEAT FOR URBAN OUTFALL SAFARI
    #h1(id = "main-title", HTML("<b>Urban Outfall Safari</b>")),  # Using HTML() for bold text

    # Grid layout for left (summary) and right (news post)
    div(
      id = "project-grid",

      # First row: Left column (Image + Text)
      div(
        class = "grid-item",
        img(src = 'Outfall example.png'), # Image
        style = "margin-bottom: 20px;", # Add space under this row
        div(
          class = "text-container",
          h2(a(
            href = "BRC urban outfall safari overview.pdf",
            target = "_blank",
            'Urban Outfall Safari overview'
          )),
          h4(HTML(
            'Follow this link to see an overview of Urban Outfall Safari technique and some advice on undertaking the survey'
          ))
        )
      ),

      # First row: Right column (Image + Text)
      div(
        class = "grid-item",
        style = "margin-bottom: 20px;", # Add space under this row
        img(src = 'Person clipboard.jpg'), # New Image
        div(
          class = "text-container",
          h2(a(
            href = "BRC Outfall Safari Print Out Survey Form.pdf",
            target = "_blank",
            'Paper copy of the Urban Outfall Safari survey form'
          )),
          h4(HTML(
            'Click this link to see a paper version of the Urban Outfall Safari survey form, which you can complete in the field before submitting the results online via the link above'
          ))
        )
      )
    ),
    #REPEAT FOR INVASIVE SPECIES
    #h1(id = "main-title", HTML("<b>Invasive species</b>")),  # Using HTML() for bold text

    # Grid layout for left (summary) and right (news post)
    div(
      id = "project-grid",

      # First row: Left column (Image + Text)
      div(
        class = "grid-item",
        style = "margin-bottom: 20px;", # Add space under this row
        img(src = 'Check clean dry invasive species.png'), # Image
        div(
          class = "text-container",
          h2(a(
            href = "BRC invasive species overview.pdf",
            target = "_blank",
            'Invasive species overview'
          )),
          h4(HTML(
            'Follow this link to see an overview of key invasive species across Birmingham and advice on how to identify them'
          ))
        )
      ),

      # First row: Right column (Image + Text)
      div(
        class = "grid-item",
        style = "margin-bottom: 20px;", # Add space under this row
        img(src = 'Person clipboard.jpg'), # New Image
        div(
          class = "text-container",
          h2(a(
            href = "BRC invasive species overview.pdf",
            target = "_blank",
            'Paper copy of the invasive species survey form'
          )),
          h4(HTML(
            'Click this link to see a paper version of the invasive species survey form, which you can complete in the field before submitting the results online via the link above'
          ))
        )
      )
    )
  )
}

#' 04_information Server Functions
#'
#' @noRd
mod_04_information_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_04_information_ui("04_information_1")

## To be copied in the server
# mod_04_information_server("04_information_1")
