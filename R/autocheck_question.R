#' Create an html part
#'
#' @param question_id unique identidier of the question
#' @param answer correct answer (can be a double or a string)
#' @param right form reaction on right answer
#' @param wrong form reaction on wrong answer
#'
#' @seealso \code{\link{autocheck_code}}
#' @author George Moroz <agricolamz@gmail.com>
#' @examples
#'
#' # ```{r, results='asis', echo=FALSE}
#' # autocheck_question(question_id = 1, answer = 5)
#' # ```
#'
#' @export
#'

autocheck_question <- function(question_id,
                               answer,
                               right = "Correct",
                               wrong = "I have a different answer") {

  if(grepl("\\.", question_id)){
    question_id <- gsub("\\.", "_", question_id)
  }

  cat(paste0(c('<form name="form_',
               question_id,
               '" onsubmit="return validate_form_',
               question_id,
               '()" method="post">',
               '<input type="text" name="answer_',
               question_id,
               '"><input type="submit" value="check"></form><br>'),
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
