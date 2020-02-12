#' Create a final part of the javascript code
#'
#' @seealso \code{\link{autocheck_question}}
#' @author George Moroz <agricolamz@gmail.com>
#' @examples
#'
#' # ```{r, results='asis', echo=FALSE}
#' # autocheck_code()
#' # ```
#'
#' @export
#'

autocheck_code <- function(){

  if(sum(duplicated(autocheck_environment$data$question_id)) > 0){
    stop("'question_id's in your questions are not unique!")}

  cat(
    paste(
      "<script>",
      paste(
        'function validate_form_',
        autocheck_environment$data$question_id,
        '() {var x = document.forms["form_',
        autocheck_environment$data$question_id,
        '"]["answer_',
        autocheck_environment$data$question_id,
        '"].value;',
        'if (x == "',
        autocheck_environment$data$answer,
        '"){',
        'alert("',
        autocheck_environment$data$right,
        '");',
        'return false;',
        '} else {',
        'alert("',
        autocheck_environment$data$wrong,
        '");',
        'return false;}}',
        sep = "",
        collapse = "\n"),
      "</script>",
      collapse = "\n"))
}
