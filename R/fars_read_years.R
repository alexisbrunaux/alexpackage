#' Create a filename and convert it in dataframe.
#'
#' This function is a combination of the 2 functions make_filename and fars_read.
#' First it creates the name of a CSV file using the 4 digits of the year argument
#' and using the functionn make_filename..
#' Then it convert the CSV file in a dataframe which we can manage in R,
#' using the function fars_read.
#' Finally with the functions mutate and select of the dplyr package, a new column
#' with the year and select the columns of the months and the year.
#' If it doesn't exist a filne for the input year, a warning message is returned.
#'
#' @param This function takes only one argument a vector of several years for the selected years,
#'      (using the \code{years} argument).
#'
#' @return This function returns a fataframe where the year and the months,
#' which are specified.
#' If it doesn't exist a filne for the input year, a warning message is returned.
#'
#' @examples
#' \dontrun{fars_read_years(c(2013:2015))}
#' \dontrun{fars_read_years(c(2015:2018))}
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}
