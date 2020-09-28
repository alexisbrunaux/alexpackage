#' Convert a CSV file in a dataframe, in order to manage it in R.
#'
#' This function can be used to convert a CSV file in a dataframe, in order
#' to manage it in R.
#' This function stops and returns a message if the file name does not exist.
#'
#' @param The only parameter of this function is the name of a CSV file which we want to read in R.
#'      (using the \code{filename} argument).
#' @return This function returns a dataframe created from a CSV data file.
#'      This function stops and returns a message if the file name does not exist.
#'
#' @examples
#' \dontrun{fars_read("data/accident_2013.csv.bz2")}
#' \dontrun{fars_read("data/accident_2018.csv.bz2")}
#'
#' @importFrom dplyr tbl_df
#' @importFrom readr read_csv
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
