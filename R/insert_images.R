#' Insert multiple images into questions and hints
#'
#' @param src image file names
#' @param alt alternate texts for an image in case If a browser cannot find an image
#' @param image_width image widths. Could be absolute value in pixels or percantage.
#' @param image_height image heights. Could be absolute value in pixels or percantage.
#'
#' @return returns the string with html code
#'
#' @author George Moroz <agricolamz@gmail.com>
#' @examples
#'
#' # ```{r, results='asis', echo=FALSE}
#' # insert_images(c("1.png", "2.png"))
#' # ```
#'
#' @export
#'

insert_images <- function(src,
                          alt = "image",
                          image_width="100%",
                          image_height="100%"){
  df <- data.frame(src, alt,
                   image_width,
                   image_height,
                   stringsAsFactors = FALSE)
  l <- lapply(seq_along(src), function(i){
    paste0(c("<img src='",
             df$src[i],
             "' alt = '",
             df$alt[i],
             "' width = '",
             df$image_width[i],
             "' height = '",
             df$image_height[i],
             "'>"),
           collapse = "")
  })
  unlist(l)
}
