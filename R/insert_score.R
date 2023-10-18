#' Create an overall sum score for all correct tasks in the document
#'
#' @return returns the html tags and javascript code
#'
#' @author George Moroz <agricolamz@gmail.com>
#'
#' @export
#'
#' @importFrom htmltools span

insert_score <- function(){
  htmltools::span(id = "checkdown_final_score", 0)
}
