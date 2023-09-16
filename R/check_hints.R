#' Create list of hints for 'rmarkdown'
#'
#' @param hint_text hint paragraph texts; can contain markdown
#' @param hint_title hint title texts; can contain markdown
#' @param hint_title_prefix string that added before each hint_title
#' @param hint_title_suffix string that added after each hint_title
#' @param list_title unique identifier for each hint
#' @param type character that describes behavior of the hints. Possible values are: \code{onclick}, \code{onmouseover}, \code{ondblclick}
#'
#' @return returns the html tags and javascript code
#'
#' @author George Moroz <agricolamz@gmail.com>
#' @examples
#'
#' check_hints(1:4)
#'
#' @export
#'
#' @importFrom markdown markdownToHTML
#' @importFrom htmltools tags
#' @importFrom htmltools tagList
#' @importFrom glue glue

check_hints <- function(hint_text,
                        hint_title = "Click here to see/close the hint",
                        hint_title_prefix = "",
                        hint_title_suffix = "",
                        list_title = "Click here to see/close the list of hints",
                        type = c("onclick", "onmouseover", "ondblclick")){

  type <- match.arg(type)

  lapply(c(hint_title_prefix, hint_title_suffix, list_title), function(argument){
    stopifnot(length(argument) == 1)
  })

  hint_title <- paste0(hint_title_prefix, hint_title, hint_title_suffix)
  df <- data.frame(hint_text, hint_title, stringsAsFactors = FALSE)

  result <- lapply(seq_along(df$hint_text), function(i){
    checkdown::check_hint(hint_text = df$hint_text[i],
                          hint_title = df$hint_title[i],
                          type = type)
  })

  list_title <- list_title |>
    markdown::markdownToHTML(text = _,
                             output = NULL,
                             fragment.only = TRUE) |>
    gsub("(<.?p>)|(\n)|(\\#)", "", x = _) |>
    htmltools::HTML()


  htmltools::tagList(
    htmltools::tags$summary(list_title),
    result) |>
  htmltools::tags$details()

}

