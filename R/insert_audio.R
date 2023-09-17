#' Insert audio
#'
#' @param src character. It specifies the location (URL) of the audio file.
#' @param controls logical. When \code{TRUE}, it specifies that audio controls should be displayed.
#' @param autoplay logical. When \code{TRUE}, the audio will automatically start playing as soon as it can do so without stopping.
#' @param loop logical. When \code{TRUE}, it specifies that the audio will start over again, every time it is finished.
#' @param muted logical. When \code{TRUE}, it specifies that the audio output should be muted.
#' @param preload character. It specifies if and how the author thinks that the audio file should be loaded when the page loads. Possible values are \code{none}, \code{auto} and \code{metadata}
#'
#' @return returns set of html tags
#'
#' @importFrom htmltools tags
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

  arguments <- c(controls = controls,
                 autoplay = autoplay,
                 loop = loop,
                 muted = muted)

  lapply(arguments, function(argument){
    stopifnot(is.logical(argument))
    stopifnot(length(argument) == 1)
  })

  result <- htmltools::tags$source(src = src) |>
    htmltools::tags$audio(preload = preload)

  result$attribs <- arguments |>
    lapply(X = _, function(i){if(i) {NA} else {NULL}}) |>
    Filter(x = _, Negate(is.null)) |>
    append(values = _, result$attribs)

  return(result)
}
