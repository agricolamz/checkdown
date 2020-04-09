#' Create check-fields and check-boxes for 'rmarkdown'
#'
#' @param question_id unique identidier of the question
#' @param answer correct answer (can be a double or a string). It is possible to put here a vector of several answers.
#' @param right form reaction on right answer
#' @param wrong form reaction on wrong answer
#' @param alignment logical argument for options' alignment: vertical if \code{TRUE}, horizontal if \code{FALSE}
#' @param options vector of values for the selection list type
#' @param button_label character value that will be displayed on the button
#' @param random_answer_order logical argument that denotes whether answers should be shuffled
#' @param type character that defines type of the list. Possible values: \code{select}, \code{radio}, \code{checkbox}
#'
#' @return returns the html and javascript code
#'
#' @author George Moroz <agricolamz@gmail.com>
#' @examples
#'
#' # ```{r, results='asis', echo=FALSE}
#' # check_question(answer = 5)
#' # ```
#'
#' @export
#'
#' @importFrom knitr is_html_output
#' @importFrom markdown markdownToHTML
#'

check_question <- function(answer,
                           right = "Correct",
                           wrong = "I have a different answer",
                           options = NULL,
                           type = "select",
                           alignment = FALSE,
                           button_label = "check",
                           random_answer_order = FALSE,
                           question_id = sample(1:1e5, 1)) {

  if(knitr::is_html_output()){
    if(grepl("\\.", question_id)){
      question_id <- gsub("\\.", "_", question_id)
    }
    right <- (markdown::markdownToHTML(text = right,
                                       output = NULL,
                                       fragment.only = TRUE))
    right <- gsub("(<.?p>)|(\n)|(\\#)", "", right)
    wrong <- (markdown::markdownToHTML(text = wrong,
                                       output = NULL,
                                       fragment.only = TRUE))
    wrong <- gsub("(<.?p>)|(\n)|(\\#)", "", wrong)
    options <- if(random_answer_order){sample(options)} else {options}
    options_value <- if(TRUE %in% grepl("^<img src=", options)){seq_along(options)} else {options}
    alignment <- ifelse(alignment, " ", "<br>")
    answer <- as.character(answer)

    if(TRUE %in% grepl("^<img src=", options) & type == "select"){
      stop('It is imposible to use images with type = "select". Please use type = "radio" or type = "checkbox"')
    }

    if(is.null(options)){
      form <- paste(c('<input type="text" name="answer_',
                      question_id,
                      '">'),
                    collapse = "")
    } else if(type == "select"){
      form <- paste(c('<select name="answer_',
                      question_id,
                      '">',
                      paste("<option>", options, "</option>"),
                      "</select>"),
                    collapse = "")
    } else if(type == "radio"){
      form <- paste0('<input type="radio" name="answer_',
                     question_id,
                     '" id="',
                     question_id,
                     '_',
                     seq_along(options),
                     '" value="',
                     options_value,
                     '"><label for="',
                     seq_along(options),
                     '">',
                     options,
                     '</label>',
                     alignment,
                     collapse = "")
    } else if(type == "checkbox"){
      form <- paste0('<input type="checkbox" id="answer_',
                     question_id,
                     '_',
                     seq_along(options),
                     '" value="',
                     options_value,
                     '"><label for="answer_',
                     question_id,
                     "_",
                     seq_along(options),
                     '">',
                     options,
                     '</label>',
                     alignment,
                     collapse = "")
    } else {
      stop("Possible values for the type variable: 'select', 'radio' or 'checkbox'")

    }
    form <- gsub(x = form, pattern = "<br>$", replacement = "")
    cat(paste0(c('<form name="form_',
                 question_id,
                 '" onsubmit="return validate_form_',
                 question_id,
                 '()" method="post">',
                 form,
                 '<br><input type="submit" value="',
                 button_label,
                 '"></form><p id="result_',
                 question_id,
                 '"></p>'),
               collapse = ""))
    if(type != "checkbox"){
      cat(
        paste(
          "<script>",
          paste(
            'function validate_form_',
            question_id,
            '() {var x, text; var x = document.forms["form_',
            question_id,
            '"]["answer_',
            question_id,
            '"].value;',
            'if (',
            paste0('x == "', answer, '"', collapse = '|'),
            '){',
            "text = '",
            right,
            "';",
            '} else {',
            "text = '",
            wrong,
            "';} document.getElementById('result_",
            question_id,
            "').innerHTML = text; return false;}",
            sep = "",
            collapse = "\n"),
          "</script>",
          collapse = "\n"))
    } else {
      cat(paste0(
        '<script> ',
        'function validate_form_',
        question_id,
        '() {',
        'var text;',
        paste0('var x',
        seq_along(options),
        ' = document.getElementById("',
        'answer_',
        question_id,
        "_",
        seq_along(options),
        '");', collapse = ""),
        'if (',
        paste0('x',
               seq_along(options),
               '.checked == ',
               tolower(options_value %in% answer),
               collapse = "&"),
        '){text = "',
        right,
        '";} else {text = "',
        wrong,
        '";} document.getElementById("result_',
        question_id,
        '").innerHTML = text; return false;}',
        '</script>'))
    }
  }
}
