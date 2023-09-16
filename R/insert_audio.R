#' Insert audio
#'
#' @param src URL
#' @param controls boolian
#' @param autoplay boolian
#' @param loop boolian
#' @param muted boolian
#' @param preload character
#'
#' @return returns set of html tags
#'
#' @importFrom htmltools tags
#' @importFrom glue glue
#'
#' @author George Moroz <agricolamz@gmail.com>
#' @examples
#'
#' insert_audio("1.wav")
#'
#' @export
#'

insert_audio <- function(src,
                         controls = TRUE,
                         autoplay = FALSE,
                         loop = FALSE,
                         muted = FALSE,
                         preload = c("none", "auto", "metadata")){

  preload <- match.arg(preload)

  lapply(c(controls, autoplay, loop, muted), function(argument){
    stopifnot(is.logical(argument))
    stopifnot(length(argument) == 1)
  })

  result <- htmltools::tags$audio(src, preload = preload)

  if(controls){
    result$attribs <- append(result$attribs, list(controls = NA))
  }

  if(autoplay){
    result$attribs <- append(result$attribs, list(autoplay = NA))
  }

  if(loop){
    result$attribs <- append(result$attribs, list(loop = NA))
  }

  if(muted){
    result$attribs <- append(result$attribs, list(loop = NA))
  }

  return(result)
}
