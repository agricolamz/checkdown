#' Create check-fields and check-boxes for 'rmarkdown'
#'
#' @param hint_text hint paragraph texts; can contain markdown
#' @param hint_title hint title texts; can contain markdown
#' @param hint_title_prefix string that added to each hint_title to the left side
#' @param hint_title_suffix string that added to each hint_title to the right side
#' @param hint_id unique identidier for each hint
#' @param list_title unique identidier for each hint
#'
#' @return returns the html and javascript code
#'
#' @author George Moroz <agricolamz@gmail.com>
#' @examples
#'
#' # ```{r, results='asis', echo=FALSE}
#' # check_hints(1:4)
#' # ```
#'
#' @export
#'
#' @importFrom knitr is_html_output
#' @importFrom markdown markdownToHTML
#'

check_hints <- function(hint_text,
                        hint_title = "Click here to see/close the hint",
                        hint_title_prefix = "",
                        hint_title_suffix = "",
                        list_title = "Click here to see/close the list of hints",
                        hint_id){
  if(knitr::is_html_output()){
    hint_title <- paste0(hint_title_prefix, hint_title, hint_title_suffix)
    df <- data.frame(hint_text, hint_title, stringsAsFactors = FALSE)
    df$hint_id <- sample(2:1e5, nrow(df))

    df$hint_text <- unlist(lapply(seq_along(df$hint_text), function(i){
      x <- as.character(df$hint_text[i])
      x <- (markdown::markdownToHTML(text = x,
                                     output = NULL,
                                     fragment.only = TRUE))
      x <- gsub("(<.?p>)|(\n)|(\\#)", "", x)
      x
    }))

    df$hint_title <- unlist(lapply(seq_along(df$hint_title), function(i){
      x <- as.character(df$hint_title[i])
      x <- (markdown::markdownToHTML(text = x,
                                     output = NULL,
                                     fragment.only = TRUE))
      x <- gsub("(<.?p>)|(\n)|(\\#)", "", x)
      x
    }))

    list_title <- (markdown::markdownToHTML(text = list_title,
                                            output = NULL,
                                            fragment.only = TRUE))
    list_title <- gsub("(<.?p>)|(\n)|(\\#)", "", list_title)

    hints <- paste0('<p id="hint_',
                    df$hint_id,
                    '", onclick="return show_hint_',
                    df$hint_id,
                    '()">',
                    df$hint_title,
                    '</p><p id="result_',
                    df$hint_id,
                    '"></p>',
                    collapse = "")

    cat(paste0(c('<p id="hint_',
                 1,
                 '", onclick="return show_hint_',
                 1,
                 '()">',
                 list_title,
                 '</p><p id="result_',
                 1,
                 '"></p>',
                 '<script> function show_hint_',
                 1,
                 '() {',
                 'var x = document.getElementById("result_',
                 1,
                 '").innerHTML;',
                 "if(!x){document.getElementById('result_",
                 1,
                 "').innerHTML = '",
                 hints,
                 "';}",
                 'else {document.getElementById("result_',
                 1,
                 '").innerHTML = "";}}',
                 paste0(
                 'function show_hint_',
                 df$hint_id,
                 '() {',
                 'var x = document.getElementById("result_',
                 df$hint_id,
                 '").innerHTML;',
                 "if(!x){document.getElementById('result_",
                 df$hint_id,
                 "').innerHTML = '",
                 df$hint_text,
                 "';}",
                 'else {document.getElementById("result_',
                 df$hint_id,
                 '").innerHTML = "";}}',
                 collapse = ""),
                 '</script>'),
               collapse = ""))
  }
}
