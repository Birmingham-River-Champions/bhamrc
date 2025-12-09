test_that("Test with_red_star works", {
  expect_s3_class(with_red_star("golem"), "shiny.tag")
  expect_equal(
    as.character(with_red_star("Enter your name here")),
    '<span>Enter your name here<span style="color:red">*</span></span>'
  )
})

test_that("Test list_to_p works", {
  expect_s3_class(
    list_to_p(c(
      "This is the first paragraph",
      "this is the second paragraph"
    )),
    "shiny.tag.list"
  )
  expect_equal(
    as.character(
      list_to_p(c(
        "This is the first paragraph",
        "this is the second paragraph"
      ))
    ),
    "<p>This is the first paragraph</p>\n<p>this is the second paragraph</p>"
  )
  expect_equal(
    as.character(
      list_to_p(
        c(
          "This is the first paragraph",
          "this is the second paragraph"
        ),
        class = "my_li"
      )
    ),
    '<p class="my_li">This is the first paragraph</p>\n<p class="my_li">this is the second paragraph</p>'
  )
})

test_that("Test make_action_button works", {
  tmp_tag <- a(href = "#", "My super link", style = "color: lightblue;")
  button <- make_action_button(
    tmp_tag,
    inputId = "mylink"
  )
  expect_s3_class(button, "shiny.tag")
  expect_equal(
    as.character(button),
    '<a href="#" style="color: lightblue;" id="mylink" class="action-button">My super link</a>'
  )
  expect_error(
    button_2 <- make_action_button(
      unclass(tmp_tag),
      inputId = "mylink_2"
    )
  )
  expect_error(
    button_3 <- make_action_button(
      button,
      inputId = "mylink_3"
    )
  )
  expect_error(
    button_4 <- make_action_button(
      tmp_tag,
      inputId = NULL
    )
  )
  tmp_tag_2 <- tmp_tag
  tmp_tag_2$attribs$id <- "id_already_present"
  expect_warning(
    button_5 <- make_action_button(
      tmp_tag_2,
      inputId = "mylink_5"
    )
  )
  tmp_tag_3 <- tmp_tag
  tmp_tag_3$attribs$class <- "class_already_present"
  button_6 <- make_action_button(
    tmp_tag_3,
    inputId = "someID"
  )
})
