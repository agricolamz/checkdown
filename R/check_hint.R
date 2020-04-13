#' Create check-fields and check-boxes for 'rmarkdown'
#'
#' @param hint_text hint paragraph text; can contain markdown
#' @param hint_title click paragraph text; can contain markdown
#' @param hint_id unique identidier of the question
#'
#' @return returns the html and javascript code
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
                       hint_title = "Click here to see/close the hint",
                       hint_id = sample(1:1e5, 1)){
  if(knitr::is_html_output()){
    hint_text <- (markdown::markdownToHTML(text = hint_text,
                                           output = NULL,
                                           fragment.only = TRUE))
    hint_text <- gsub("(<.?p>)|(\n)|(\\#)", "", hint_text)
    hint_title <- as.character(hint_title)
    hint_title <- (markdown::markdownToHTML(text = hint_title,
                                            output = NULL,
                                            fragment.only = TRUE))
    hint_title <- gsub("(<.?p>)|(\n)|(\\#)", "", hint_title)

    cat(paste0(c('<p id="hint_',
                 hint_id,
                 '", onclick="return show_hint_',
                 hint_id,
                 '()">',
                 hint_title,
                 '</p><p id="result_',
                 hint_id,
                 '"></p>',
                 '<script> function show_hint_',
                 hint_id,
                 '() {',
                 'var x = document.getElementById("result_',
                 hint_id,
                 '").innerHTML;',
                 "if(!x){document.getElementById('result_",
                 hint_id,
                 "').innerHTML = '",
                 hint_text,
                 "';}",
                 'else {document.getElementById("result_',
                 hint_id,
                 '").innerHTML = "";}}',
                 '</script>'),
               collapse = ""
    ))
  }
}
