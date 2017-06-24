#' Read csv.bz2 file and create a data frame tbl.
#'
#' This is a function that read a csv.bz2 file. You can
#' manually write the name of the csv.bz2 file in the filename parameter
#' and it is utilized in conjuntion with the \code{make_filename} function for files obtain
#' from the US National Highway Traffic Safety Administration's that have this
#' nomenclature "accident_%d.csv.bz2" where %d represent a year.
#' It will return a tibble object that have all these classes: 'tbl_df', 'tbl' and 'data.frame'.
#' The function return an error message if the file does not exist or if it is not find.
#'
#' @param filename A csv.bz2 file that can be explicitly specify
#'    or from a string "character" generated with the \code{make_filename} function of this package.
#'
#' @return This function returns the dataframe tbl from an input that is a csv.bz2 file.
#'
#' @examples
#' \dontrun{
#' data <- fars_read(filename = "./accident_2013.csv.bz2")
#'
#' file<-make_filename(year=2013)
#' data<-fars_read(filename = file)
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
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

#' Make a filename for a specific year.
#'
#' This function create a filename of the form "accident_%d.csv.bz2" where %d specify a year.
#' It used the \code{sprintf} base function that returns a character
#' string containing a formatted combination of text and variable values.
#'
#' @param year A year, numeric integer value in the form yyyy.
#'
#' @return This function returns a character string of the form "accident_%d.csv.bz2" where %d specify a year.
#'
#' @examples
#' file<-make_filename(year=2013)
#'
#' @export
make_filename <- function(year) {
    year <- as.integer(year)
    sprintf("accident_%d.csv.bz2", year)
}

#' Extract month and year for all observations from an accident file.
#'
#'This function extract the month and the year for each observation (accident) of a file and
#'put the results in a list. In the list there is one element for each year and each element is a tibble
#'with 2 columns (MONTH and year) for each accident.
#'
#' @param years One or more years, numeric integer value specified in form of a list.
#'
#' @return This function returns a list, one element for each specified year and each element is a tibble
#'    with 2 columns (MONTH and year), the number of observations is the number of accidents in the year.
#'    If the parameters are not legal integers the function return an error.
#'    If a specified year is not valid year, the function returns a warning message indicating the invalid year.
#'
#' @examples
#' \dontrun{
#' read_years<-fars_read_years(years=2013:2015)
#' }
#'
#' @importFrom dplyr mutate select %>%
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

#' Number of accidents per months/year(s)
#'
#' This function returns a summary of the number of accidents by month and for each specified year.
#'
#' @inheritParams fars_read_years
#'
#' @return This function returns a summary of the number of accidents by month and for each specified year.
#'    It will return a tibble object that have all these classes: 'tbl_df', 'tbl' and 'data.frame'.
#'    One row for each month, the month number in the first column (MONTH) and for each specified year a column with
#'    named from the year (`yyyy`) containing for each month the number of accidents.
#'    If one or more specified years is not valid, the function return an error and specify the invalid year.
#'
#' @examples
#' \dontrun{
#' summarize_years<-fars_summarize_years(years=2013:2015)
#' }
#'
#' @importFrom dplyr bind_rows group_by summarize %>%
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

#' Produce a plot of the location of the accidents on a state map.
#'
#' This function returns the location of the accidents for a specified state and year.
#'
#' @param state.num The state number, numeric integer value.
#' @param year A year, numeric value in the form yyyy of the accidents.
#'
#' @return This function returns a plot of a state map with points representing the location of the accidents in the year.
#'    If the state is not a valid number or it is not found in the data then the function stops and returns an error
#'    with the message "invalid STATE number: ". If there is no accident for the specified state and year then the
#'    function returns the message "no accidents to plot".
#'
#' @examples
#' \dontrun{
#' fars_map_state(40, 2014)
#' }
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
