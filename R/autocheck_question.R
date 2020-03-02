#' Create check-fields and check-boxes for 'rmarkdown'
#'
#' @param question_id unique identidier of the question
#' @param answer correct answer (can be a double or a string)
#' @param right form reaction on right answer
#' @param wrong form reaction on wrong answer
#' @param options vector of values for the selection list type
#'
#' @author George Moroz <agricolamz@gmail.com>
#' @examples
#'
#' # ```{r, results='asis', echo=FALSE}
#' # autocheck_question(question_id = 1, answer = 5)
#' # ```
#'
#' @export
#'
#' @importFrom knitr is_html_output

autocheck_question <- function(question_id,
                               answer,
                               right = "Correct",
                               wrong = "I have a different answer",
                               options = NULL) {

  if(knitr::is_html_output()){
    if(grepl("\\.", question_id)){
      question_id <- gsub("\\.", "_", question_id)
    }

    if(is.null(options)){
      form <- paste(c('<input type="text" name="answer_',
                      question_id,
                      '">'),
                    collapse = "")
    } else {
      form <- paste(c('<select name="answer_',
                      question_id,
                      '">',
                      paste("<option>", options, "</option>"),
                      "</select>"),
                    collapse = "")
    }

    cat(paste0(c('<form name="form_',
                 question_id,
                 '" onsubmit="return validate_form_',
                 question_id,
                 '()" method="post">',
                 form,
                 '<input type="submit" value="check"></form><br>'),
               collapse = ""))
    cat(
      paste(
        "<script>",
        paste(
          'function validate_form_',
          question_id,
          '() {var x = document.forms["form_',
          question_id,
          '"]["answer_',
          question_id,
          '"].value;',
          'if (x == "',
          answer,
          '"){',
          'alert("',
          right,
          '");',
          'return false;',
          '} else {',
          'alert("',
          wrong,
          '");',
          'return false;}}',
          sep = "",
          collapse = "\n"),
        "</script>",
        collapse = "\n"))
  }
}
