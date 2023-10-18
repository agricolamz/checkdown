#' Create a temporal file for example list when the package is loaded
#'
#' @author George Moroz <agricolamz@gmail.com>
#' @noRd
#'
#' @importFrom utils write.table

.onLoad <- function(libname = find.package("checkdown"),
                    pkgname = "checkdown") {
  tmp_file <- tempfile(pattern = "checkdown.table", fileext = ".csv")
  options("checkdown.table" = tmp_file)
  utils::write.table(data.frame(q_id = "q_id", right = "right"),
              file = getOption("checkdown.table"),
              col.names = FALSE,
              row.names = FALSE,
              sep = ",")

  invisible()
}

#' Insert code for insert_score() function
#'
#' @author George Moroz <agricolamz@gmail.com>
#' @noRd
#'
#' @importFrom htmltools span
#' @importFrom glue glue
#' @importFrom utils read.csv

insert_js_code_for_score_calculation <- function() {
  df <- utils::read.csv(getOption("checkdown.table"), header = TRUE)
  df$rows_id <- seq_along(df$q_id)
  paste0("function evaluate_final_score(){",
         paste0("var text, ", paste0("res", df$rows_id, collapse = ", "), ";"),
         paste0(glue::glue_data(df, "res{rows_id} = document.getElementById('result_{q_id}').innerText == '{right}';"), collapse = " "),
         paste0("text = ", paste0("res", df$rows_id, collapse = " + "), ";"),
         "document.getElementById('checkdown_final_score').innerHTML = text;
         return false;}")
}
