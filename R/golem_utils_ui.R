#' Turn an R list into corresponding HTML paragraph tags
#'
#' @param list an R list
#' @param class a class for the paragraph tags
#'
#' @return An HTML tag
#' @noRd
#'
#' @examples
#' list_to_p(c("This is the first paragraph", "this is the second paragraph"))
#' @importFrom shiny tags tagAppendAttributes tagList
#'
list_to_p <- function(list, class = NULL) {
  if (is.null(class)) {
    tagList(
      lapply(
        list,
        tags$p
      )
    )
  } else {
    res <- lapply(
      list,
      tags$p
    )
    res <- lapply(
      res,
      function(x) {
        tagAppendAttributes(
          x,
          class = class
        )
      }
    )
    tagList(res)
  }
}

#' Add a red star at the end of the text
#'
#' Adds a red star at the end of the text
#' (for example for indicating mandatory fields).
#'
#' @param text the HTLM text to put before the red star
#'
#' @return an html element
#' @noRd
#'
#' @examples
#' with_red_star("Enter your name here")
#' @importFrom shiny tags HTML
with_red_star <- function(text) {
  shiny::tags$span(
    HTML(
      paste0(
        text,
        shiny::tags$span(
          style = "color:red",
          "*"
        )
      )
    )
  )
}

#' Make the current tag behave like an action button
#'
#' Only works with compatible tags like button or links
#'
#' @param tag Any compatible tag.
#' @param inputId Unique id. This will host the input value to be used
#' on the server side.
#'
#' @return The modified tag with an extra id and the action button class.
#' @noRd
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   link <- a(href = "#", "My super link", style = "color: lightblue;")
#'
#'   ui <- fluidPage(
#'     make_action_button(link, inputId = "mylink")
#'   )
#'
#'   server <- function(input, output, session) {
#'     observeEvent(input$mylink, {
#'       showNotification("Pouic!")
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
make_action_button <- function(tag, inputId = NULL) {
  # some obvious checks
  if (!inherits(tag, "shiny.tag")) {
    stop("Must provide a shiny tag.")
  }
  if (!is.null(tag$attribs$class)) {
    if (isTRUE(grepl("action-button", tag$attribs$class))) {
      stop("tag is already an action button")
    }
  }
  if (is.null(inputId) && is.null(tag$attribs$id)) {
    stop(
      "tag does not have any id. Please use inputId to be able to
           access it on the server side."
    )
  }

  # handle id
  if (!is.null(inputId)) {
    if (!is.null(tag$attribs$id)) {
      warning(
        paste(
          "tag already has an id. Please use input$",
          tag$attribs$id,
          "to access it from the server side. inputId will be ignored."
        )
      )
    } else {
      tag$attribs$id <- inputId
    }
  }

  # handle class
  if (is.null(tag$attribs$class)) {
    tag$attribs$class <- "action-button"
  } else {
    tag$attribs$class <- paste(tag$attribs$class, "action-button")
  }
  # return tag
  tag
}
