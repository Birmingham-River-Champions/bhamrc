#' This function creates information cards on the page for a given
#' set of text, image, and links
#' @param title The title of the link
#' @param description The description underneath the link
#' @param alt_text Alternative text for the provided image, defaults to NULL
#' @param url The URL that the header link should target
#' @param image_src The location within the app of the image
information_card <- function(
    title,
    description,
    alt_text = NULL,
    url,
    image_src,
    ns
) {
    # Use alt_text if provided, otherwise fall back to title for accessibility
    alt_attribute <- if (!is.null(alt_text)) alt_text else title
    div(
        class = "grid-item",
        img(src = image_src, alt = alt_text), # Image
        div(
            class = "text-container",
            h4(actionLink(
                inputId = ns(title),
                label = title,
                onclick = url # url is the action to open a window to the URL unless this is a link to the Submission form panel
            )),
            h5(HTML(description))
        )
    )
}
