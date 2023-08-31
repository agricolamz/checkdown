#' Create check-fields and check-boxes for 'rmarkdown'
#'
#' @param answer correct answer (can be a double or a string). It is possible to put here a vector of several answers.
#' @param right form reaction on right answer
#' @param wrong form reaction on wrong answer
#' @param options vector of values for the selection list type
#' @param type character that defines type of the list. Possible values: \code{text}, \code{select}, \code{radio}, \code{checkbox}, \code{in_order}
#' @param button_label character value that will be displayed on the button
#' @param placeholder character that defines a short hint that describes the expected value of an input field. This works with the \code{text} input type only.
#' @param alignment logical argument for options' alignment: \code{vertical} or \code{horizontal}
#' @param random_answer_order logical argument that denotes whether answers should be shuffled, when the \code{type} value is \code{select}, \code{radio} or \code{checkbox}
#' @param width_of_in_order character with some values for width of the boxes, when the \code{type} value is \code{in_order}. Possible values: "30px", "20\%"
#' @param height_of_in_order character with some values for height of the boxes, when the \code{type} value is \code{in_order}. Possible values: "30px", "20\%"
#' @param style_of_in_order character that contains CSS style for the \code{div} boxes, when the \code{type} value is \code{in_order}
#' @param q_id unique identifier of the question
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
#' @importFrom htmltools div
#' @importFrom htmltools tagList
#' @importFrom glue glue

check_question <- function(answer,
                           right = "Correct",
                           wrong = "I have a different answer",
                           options = NULL,
                           type = c("text", "select", "radio", "checkbox", "in_order"),
                           button_label = "check",
                           alignment = c("vertical", "horizontal"),
                           placeholder = "",
                           random_answer_order = FALSE,
                           width_of_in_order = paste0(round(1/length(answer)*85), "%"),
                           height_of_in_order = "60px",
                           style_of_in_order = "padding:5px;border: 1px solid #aaaaaa; display: inline-block;",
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
  } else if(type == "in_order" & is.null(options)){

    if(length(answer) < 2){
      stop("For tasks of type 'in_order' answer vector's length should be greater then 1")
    }

    answer_sample <- seq_along(answer)

    while(identical(answer_sample, seq_along(answer))){
      answer_sample <- sample(seq_along(answer))
    }

    answers <- lapply(answer_sample, function(i){

      if(which(i == answer_sample) != length(answer_sample)){
        htmltools::tagList(htmltools::tags$td(id = glue::glue("answer_{q_id}_{i}"),
                                              style = "padding:5px; border:1px solid #ccc;",
                                              answer[i]),
                           htmltools::tags$button(type="button",
                                                  onclick = "swapElements(this);",
                                                  "\u21ff") |>
                             htmltools::tags$td(style = "padding:5px"))
      } else {
        htmltools::tags$td(id = glue::glue("answer_{q_id}_{i}"),
                                              style = "padding:5px; border:1px solid #ccc;",
                                              answer[i])
      }
      })

    UI_part <- answers |>
      htmltools::tags$tr(id = glue::glue("task_{q_id}")) |>
      htmltools::tags$table()
  }

  question <- htmltools::tagList(
    UI_part,
    htmltools::tags$input(type = "submit", value = button_label),
    htmltools::tags$div(id = glue::glue("result_{q_id}"))) |>
    htmltools::tags$form(name = glue::glue("form_{q_id}"),
                         onsubmit = glue::glue("return validate_form_{q_id}()"),
                         method = "post")

# javascript part ---------------------------------------------------------

  if(!(type %in% c("checkbox", "in_order"))){

    answer <- paste0("x == '", answer, "'", collapse = '|')

    js_script <- glue::glue("<script>function validate_form_{q_id}() {{var x, text; var x = document.forms['form_{q_id}']['answer_{q_id}'].value;if ({answer}){{text = '{right}';}} else {{text = '{wrong}';}} document.getElementById('result_{q_id}').innerHTML = text; return false;}}</script>")

  } else if(type == "checkbox"){

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
  } else if(type == "in_order"){

    condition <- lapply(seq_along(answer), function(i){
      j = (i-1)*2
      glue::glue("ch[{j}].id == 'answer_{q_id}_{i}'")
    }) |>
      unlist() |>
      paste0(collapse = "&")

    js_check_question <- glue::glue("function validate_form_{q_id}() {{var text;var ch = document.getElementById('task_{q_id}').children; if({condition}){{text = '{right}';}} else {{text = '{wrong}';}} document.getElementById('result_{q_id}').innerHTML = text; return false;}}")
    js_swap <- "function swapElements(element) {const parent = element.parentNode; const afterNode = parent.nextElementSibling; const beforeNode = parent.previousElementSibling; parent.insertAdjacentElement('beforebegin', afterNode); parent.insertAdjacentElement('afterend', beforeNode);}"
    js_script <- glue::glue("<script>{js_check_question}{js_swap}</script>")
  }

  htmltools::tagList(question, htmltools::HTML(js_script))
}
