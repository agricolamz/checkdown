#' Create check-fields and check-boxes for 'rmarkdown'
#'
#' @param q_id unique identifier of the question
#' @param answer correct answer (can be a double or a string). It is possible to put here a vector of several answers.
#' @param right form reaction on right answer
#' @param wrong form reaction on wrong answer
#' @param alignment logical argument for options' alignment: \code{vertical} or \code{horizontal}
#' @param options vector of values for the selection list type
#' @param button_label character value that will be displayed on the button
#' @param placeholder character that defines a short hint that describes the expected value of an input field. This works with the \code{text} input type only.
#' @param random_answer_order logical argument that denotes whether answers should be shuffled
#' @param type character that defines type of the list. Possible values: \code{text}, \code{select}, \code{radio}, \code{checkbox}
#'
#' @return returns the html tags and javascript code
#'
#' @author George Moroz <agricolamz@gmail.com>
#' @examples
#'
#' # ```{r, echo=FALSE}
#' # check_question(answer = 5)
#' # ```
#'
#' @export
#'
#' @importFrom markdown markdownToHTML
#' @importFrom htmltools tags
#' @importFrom htmltools tagList
#' @importFrom glue glue

check_question <- function(answer,
                           right = "Correct",
                           wrong = "I have a different answer",
                           options = NULL,
                           type = c("text", "select", "radio", "checkbox"),
                           button_label = "check",
                           alignment = c("vertical", "horizontal"),
                           placeholder = "",
                           random_answer_order = FALSE,
                           q_id = sample(1:1e5, 1)) {

# polish arguments --------------------------------------------------------

  type <- match.arg(type)
  alignment <- match.arg(alignment)

  if(grepl("\\.", q_id)){
    q_id <- gsub("\\.", "_", q_id)
  }

  right <- right |>
    markdown::markdownToHTML(text = _,
                             output = NULL,
                             fragment.only = TRUE) |>
    gsub("(<.?p>)|(\n)|(\\#)", "", x = _) |>
    htmltools::HTML()

  wrong <- wrong |>
    markdown::markdownToHTML(text = _,
                             output = NULL,
                             fragment.only = TRUE) |>
    gsub("(<.?p>)|(\n)|(\\#)", "", x = _) |>
    htmltools::HTML()

  placeholder <- as.character(placeholder[1])

  options <- if(random_answer_order){sample(options)} else {options}

  answer <- if(!is.null(answer)){answer |> as.character() |> unique()}

# form part ---------------------------------------------------------------

  if(type == "text" & is.null(options)){

    UI_part <- htmltools::tags$input(type = "text",
                                     placeholder = placeholder,
                                     name = glue::glue("answer_{q_id}"))

  } else if(type == "select" & !is.null(options)){

    select_options <- lapply(options, function(i){
      htmltools::tagList(htmltools::tags$option(i))})

    UI_part <- htmltools::tags$select(name = glue::glue("answer_{q_id}"),
                                      select_options)

  } else if(type == "radio" & !is.null(options)){

    UI_part <- lapply(seq_along(options), function(i){
      htmltools::tagList(
        htmltools::tags$input(type = type,
                              name = glue::glue("answer_{q_id}"),
                              id = glue::glue("answer_{q_id}_{i}"),
                              value = ifelse(class(options[i]) == "list", i, options[i])),
        htmltools::tags$label(options[i]),
        if(alignment == "vertical"){htmltools::tags$br()})
    })

  } else if(type == "checkbox" & !is.null(options)){

    UI_part <- lapply(seq_along(options), function(i){
      htmltools::tagList(
        htmltools::tags$input(type = type,
                              id = glue::glue("answer_{q_id}_{i}"),
                              value = options[i]),
        htmltools::tags$label(options[i]),
        if(alignment == "vertical"){htmltools::tags$br()})
    })
  }

  question <- htmltools::tagList(
    UI_part,
    htmltools::tags$input(type = "submit", value = button_label),
    htmltools::tags$div(id = glue::glue("result_{q_id}"))) |>
    htmltools::tags$form(name = glue::glue("form_{q_id}"),
                         onsubmit = glue::glue("return validate_form_{q_id}()"),
                         method = "post")

# javascript part ---------------------------------------------------------

  if(type != "checkbox"){

    answer <- paste0("x == '", answer, "'", collapse = '|')

    js_script <- glue::glue("<script>function validate_form_{q_id}() {{var x, text; var x = document.forms['form_{q_id}']['answer_{q_id}'].value;if ({answer}){{text = '{right}';}} else {{text = '{wrong}';}} document.getElementById('result_{q_id}').innerHTML = text; return false;}}</script>")

  } else {

    vars <- lapply(seq_along(options), function(i){
      glue::glue("var x{i} = document.getElementById('answer_{q_id}_{i}');")
    }) |>
      unlist() |>
      paste0(collapse = " ")

    condition <- lapply(seq_along(options), function(i){
      condition_value <- tolower(options[i] %in% answer)
      glue::glue("x{i}.checked == {condition_value}")
    }) |>
      unlist() |>
      paste0(collapse = "&")

    js_script <- glue::glue("<script>function validate_form_{q_id}() {{var text; {vars} if ({condition}){{text = '{right}';}} else {{text = '{wrong}';}} document.getElementById('result_{q_id}').innerHTML = text; return false;}}</script>")
  }

  htmltools::tagList(question, htmltools::HTML(js_script))
}
