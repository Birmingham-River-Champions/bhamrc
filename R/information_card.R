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
    image_src
) {
    # Use alt_text if provided, otherwise fall back to title for accessibility
    alt_attribute <- if (!is.null(alt_text)) alt_text else title
    div(
        class = "grid-item",
        img(src = image_src, alt = alt_text), # Image
        div(
            class = "text-container",
            h2(a(
                href = url,
                target = "_blank",
                title
            )),
            h4(HTML(description))
        )
    )
}
