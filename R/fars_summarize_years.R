#' Summarize the number of accidents
#'
#' This function allows to summarize the number of events (accidents) per months and year.
#' First it reads the CSV file of the selected years and convert it in a dataframe
#' using the function above: fars_read_years.
#' Then it binds the columns and group the lines per month and years using the package deplyr.
#' Finally, it summarize the number of events (accidents) per month and years using the package deplyr.
#' And reorganize the dataframe using the function spread of the package tidyr.
#'
#' @param This function takes only one argument a vector of several years for the selected years,
#'      (using the \code{years} argument).
#'
#' @return This function returns a dataframe, which summarizes the number of events
#'      (accidents) per year.
#'
#' @examples
#' \dontrun{fars_summarize_years(c(2013:2015))}
#' \dontrun{fars_summarize_years(c(2015:2018))}
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows group_by summarize n
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
