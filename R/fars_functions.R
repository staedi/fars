utils::globalVariables(c("MONTH","STATE","year"))

#' fars_read(): Reads FARS (Fatality Analysis Reporting System) Data
#'
#' This function accepts a filename and reads FARS file (csv format) and creates dataframe table.
#' The function checks if the file exists and prints error msg when it doesn't.
#'
#' @param filename String type file name provided to be read into dataframe table
#'
#' @return This function returns a dplyr::tbl_df class object.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @export
#'
#' @examples
#' \dontrun{
#' fars_read(make_filename(2013))
#' }
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' make_filename(): Generates filename per given year
#'
#' This function accepts a specific year and generates a filename based on that parameter.
#' This result will be read into fars_read()
#'
#' @param year An year of interest to be made for the accident filename (2013-2015)
#'
#' @return Filename will be returned
#' @export
#'
#' @examples
#' make_filename(2013)
make_filename <- function(year) {
        year <- as.integer(year)
        fp <- sprintf("accident_%d.csv.bz2", year)
        system.file('extdata',fp,package='fars')
}

#' fars_read_years(): Read accidents data with specific year
#'
#' This function accepts a specific year and generates an accident filename of that year to be read.
#' If the file doesn't exist, warning msg will be displayed.
#' Otherwise, that file of accidents will be read and a list with only MONTH and year column will be returned
#'
#' @param years An year of interest in which data to be read (2013-2015)
#'
#' @return A list of MONTH and year from the accidents dataset of given year
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' fars_read_years(c(2013,2014,2015))
#' }
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

#' fars_summarize_years(): Summarize monthly accidents for given year
#'
#' This function accepts a specific year and reads the accident data of that year.
#' Afterwards, summary of accidents per month of given year will be displayed
#'
#' @param years An year of interest in which data to be read (2013-2015)
#'
#' @return A data table summarizing the number of accidents per month
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom tidyr spread
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013,2014))
#' }
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = dplyr::n()) %>%
                tidyr::spread(year, n)
}

#' fars_map_state(): Maps accident data of specific state in a given year
#'
#' This function accepts a specific year and a numeric value of US State.
#' Afterwards, the accident data of that year is read.
#' If the state number is invalid, error msg is displayed.
#' Otherwise, accident out of that given state is filtered out.
#' Further, when no accidents data are available, 'no accidents to plot' is to be shown.
#' Otherwise, accidents data is to be mapped within the state geography.
#'
#' @param state.num Numeric value of US State (Min:1, Max:56)
#' @param year An year of interest in which data to be read (2013-2015)
#'
#' @return Displays a plot of state map with accidents data. If there's no accidents in the state, 'no accidents to plot' is printed instead.
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @export
#'
#' @examples
#' \dontrun{
#' fars_map_state(1,2013)
#' }
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
