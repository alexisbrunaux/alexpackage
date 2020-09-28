#' Build filename with the year
#'
#' This filename will be used in the function above fars_read.
#'
#' @param This function takes only one argument a 4 digits number for the year,
#'      (using the \code{year} argument).
#'
#' @return This function returns simply the name of the CSV file, which we want
#'      to convert in dataframe with the function above fars_read.
#'
#' @examples
#' \dontrun{make_filename(2013)}
#' \dontrun{make_filename(2018)}
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
