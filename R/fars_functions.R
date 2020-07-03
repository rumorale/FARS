#' Print "fars_read"
#'
#' This is a simple function that trasform your data in data frame tbl.
#'
#' @param filename A character string giving path file
#'
#' @return This function returns a data frame tbl
#'
#' @details this function may fail if file is not supported by readr::read_csv#'
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' fars_read(filename)
#'}
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

#' Print "make_filename"
#'
#' This is a simple function that build file names using year
#'
#' @param year A int value contained in file name
#'
#' @return This function returns a data frame tbl
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' make_filename(2020)
#'}
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Print "fars_read_years"
#'
#' This is a simple function return data contains MONTH and year
#'
#' @param years A int vector with years contained in the file name
#'
#' @return This function returns a list with data contains MONTH and year
#'
#' @details This function fails when the file whose year is not in years
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @examples
#' \dontrun{
#' years <- 2013:2014
#' fars_read_years(years)
#'}
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

#' Print "fars_summarize_years"
#'
#' This is a simple function summarize observations number by month and year
#'
#' @param years A int vector with years contained in the file name
#'
#' @return This function returns a wide table that contain number observation by month and year
#'
#' @details If years does not contain the year in the file name the function fails
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr bind_rows
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{
#' years <- 2013:2015
#' fars_summarize_years(years)
#'}
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Print "fars_map_state"
#'
#' This is a simple function that graphs geographic points given the state number and the year
#'
#' @param state.num A int value that represent a state
#'
#' @param year A int value contained in file name
#'
#' @return This function returns a plot with points that are composed of LATITUDE and LONGITUD in state.num and year
#'
#' @details If state.num or year are not in the data then this function fails
#'
#' @importFrom dplyr filter
#' @importFrom graphics points
#' @importFrom maps map
#'
#' @examples
#' \dontrun{
#' year <- 2013
#' state.num <- 1
#' fars_map_state(state.num, year)
#'}
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
