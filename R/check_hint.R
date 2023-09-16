#' Create hint for 'rmarkdown'
#'
#' @param hint_text hint paragraph text; can contain markdown
#' @param hint_title click paragraph text; can contain markdown
#' @param hint_id unique identifier of the question
#' @param type character that describes behavior of the hint. Possible values are: \code{onclick}, \code{onmouseover}, \code{ondblclick}
#'
#' @return returns the html tags and javascript code
#'
#' @author George Moroz <agricolamz@gmail.com>
#' @examples
#'
#' check_hint("You can use argument `echo=FALSE`!")
#'
#' @export
#'
#' @importFrom markdown markdownToHTML
#' @importFrom htmltools tags
#' @importFrom htmltools tagList
#' @importFrom glue glue

check_hint <- function(hint_text,
                       hint_title = "Click here to see/close the hint",
                       type = c("onclick", "onmouseover", "ondblclick"),
                       hint_id = sample(1e5:1, 1)){

  lapply(c(hint_text, hint_title), function(argument){
    stopifnot(length(argument) == 1)
  })

  type <- match.arg(type)

  if(grepl("\\.", hint_id)){
      hint_id <- gsub("\\.", "_", hint_id)
  }

  hint_text <- hint_text |>
      markdown::markdownToHTML(text = _,
                               output = NULL,
                               fragment.only = TRUE) |>
      gsub("(<.?p>)|(\n)|(\\#)", "", x = _) |>
      htmltools::HTML()

    hint_title <- hint_title |>
      as.character() |>
      markdown::markdownToHTML(text = _,
                               output = NULL,
                               fragment.only = TRUE) |>
      gsub("(<.?p>)|(\n)|(\\#)", "", x = _) |>
      htmltools::HTML()

    if(type == "onclick"){

    htmltools::tagList(
      htmltools::div(id = glue::glue('hint_{hint_id}'),
                     onclick = glue::glue('return show_hint_{hint_id}()'),
                     hint_title),
      htmltools::div(id = glue::glue('result_{hint_id}'),
                     onclick = glue::glue('return show_hint_{hint_id}()')),
      htmltools::HTML(glue::glue("<script>function show_hint_{hint_id}(){{var x = document.getElementById('result_{hint_id}').innerHTML; if(!x){{document.getElementById('result_{hint_id}').innerHTML = '{hint_text[[1]]}';}} else {{document.getElementById('result_{hint_id}').innerHTML = '';}}}}</script>")))
    } else if(type == "ondblclick"){
      htmltools::tagList(
        htmltools::div(id = glue::glue('hint_{hint_id}'),
                       ondblclick = glue::glue('return show_hint_{hint_id}()'),
                       hint_title),
        htmltools::div(id = glue::glue('result_{hint_id}'),
                       ondblclick = glue::glue('return show_hint_{hint_id}()')),
        htmltools::HTML(glue::glue("<script>function show_hint_{hint_id}(){{var x = document.getElementById('result_{hint_id}').innerHTML; if(!x){{document.getElementById('result_{hint_id}').innerHTML = '{hint_text[[1]]}';}} else {{document.getElementById('result_{hint_id}').innerHTML = '';}}}}</script>")))
    } else if(type == "onmouseover"){
      htmltools::tagList(
        htmltools::div(id = glue::glue('hint_{hint_id}'),
                       onmouseover = glue::glue('return show_hint_{hint_id}()'),
                       hint_title),
        htmltools::div(id = glue::glue('result_{hint_id}'),
                       onmouseover = glue::glue('return show_hint_{hint_id}()')),
        htmltools::HTML(glue::glue("<script>function show_hint_{hint_id}(){{var x = document.getElementById('result_{hint_id}').innerHTML; if(!x){{document.getElementById('result_{hint_id}').innerHTML = '{hint_text[[1]]}';}} else {{document.getElementById('result_{hint_id}').innerHTML = '';}}}}</script>")))
    }
}
