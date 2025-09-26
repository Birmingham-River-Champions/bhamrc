information_card <- function(
    title,
    description,
    alt_text = NULL,
    url,
    img_src
) {
    # Use alt_text if provided, otherwise fall back to title for accessibility
    alt_attribute <- if (!is.null(alt_text)) alt_text else title
    div(
        class = "grid-item",
        img(src = img_src, alt = alt_text), # Image
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
