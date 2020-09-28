#' Build a map where the events (accidents) occurs
#'
#' This function converts a CSV file in a dataframe and select a specified year and state number,
#' then using this dataframe builds a map in order to indicate where the accidents occurs and how many accidents occurs in one place.
#'
#' @param this function take two arguments, first the \code{state.num} argument, which speciies
#'     the state which we are interested.
#' @param the second argument is the \code{year} argument which specifies the year which we are interested.
#'
#' @return this function returns a map where we can see how many accidents occurs and where they occurs.
#'     If the input state number doesn't exist in the data the function running stops
#'     and the message "invalid STATE number: " is returned
#'     If there is not data for specified state number and year
#'     the message "no accidents to plot" is returned.
#'
#' @examples
#' \dontrun{fars_map_state(c(18,2013))}
#' \dontrun{fars_map_state(c(18,2018))}
#' \dontrun{fars_map_state(c(82:2013))}
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
