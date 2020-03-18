#' Create check-fields and check-boxes for 'rmarkdown'
#'
#' @param hint_text hint paragraph text; can contain markdown
#' @param click_text click paragraph text; can contain markdown
#' @param hint_id unique identidier of the question
#'
#' @author George Moroz <agricolamz@gmail.com>
#' @examples
#'
#' # ```{r, results='asis', echo=FALSE}
#' # check_hint("You can use argument `asis`!")
#' # ```
#'
#' @export
#'
#' @importFrom knitr is_html_output
#' @importFrom markdown markdownToHTML
#'

check_hint <- function(hint_text,
                       click_text = "Click here to see/close the hint",
                       hint_id = sample(1:1e5, 1)){
  if(knitr::is_html_output()){
    hint_text <- (markdown::markdownToHTML(text = hint_text,
                                           output = NULL,
                                           fragment.only = TRUE))
    hint_text <- gsub("(<.?p>)|(\n)|(\\#)", "", hint_text)
    click_text <- (markdown::markdownToHTML(text = click_text,
                                            output = NULL,
                                            fragment.only = TRUE))
    click_text <- gsub("(<.?p>)|(\n)|(\\#)", "", click_text)

    cat(paste0(c('<p id="hint_',
                 hint_id,
                 '", onclick="return show_hint_',
                 hint_id,
                 '()">',
                 click_text,
                 '</p><p id="result_',
                 hint_id,
                 '"></p>',
                 '<script> function show_hint_',
                 hint_id,
                 '() {',
                 'var x = document.getElementById("result_',
                 hint_id,
                 '").innerHTML;',
                 'if(!x){document.getElementById("result_',
                 hint_id,
                 '").innerHTML = "',
                 hint_text,
                 '";}',
                 'else {document.getElementById("result_',
                 hint_id,
                 '").innerHTML = "";}}',
                 '</script>'),
                 collapse = ""
                 ))
  }
}
