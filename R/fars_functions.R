#'Read and save data into a dataframe
#'
#'This is a simple function that takes the file name is parameter. If there exists
#'such file, the function will read the file ( \code{readr::read_csv}) and save
#' the data in the file as a dataframe(\code{dplyr::tbl_df}). If the file name does
#' not exist, the function will return an error stating that such file does not
#' exist(\code {stop()}).
#'
#'@param filename The input by the user. A character string giving the name of the
#'  file to read
#'@param data The content from reading the file.This is later turned into a dataframe
#'
#'@return This function will return the material read from the file into a dataframe.
#'   No specific print out will the function produce.
#'
#'@importFrom
#'library(readr)
#'library(dplyr)
#'
#'@examples
#'fars_read('Sherry.txt')
#'fars_read('accident_2013.csv')
#'
#'@export
library(readr)
library(dplyr)
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#'Make file name with year
#'
#'This is a function that takes the year as input. After turning it from a string
#'into an integer(\code{as.integer()}), it prints out a file name in the form of
#'"accident_n.csv.bz2",where n will be the year the user puts in.
#'
#'@param year A character string given by the user that will be part of the file name
#'
#'@examples
#'make_filename(2006)
#'make_filename(1930)
#'
#'@export

make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read multiple data and extract the month and year part
#'
#' This is a function that takes a vector of years as input. For each element in
#' the vector, first, it makes the file name with the given year (using the
#' \code{make_filename()} function). Then it tries to read in the data (using \code
#' {fars_read()} function and \code{dat} argument). For the data in the file, the
#' function will select the month of the elements where variable 'year' is
#' equal to the current element in the years vector (with the \code{tryCatch()}
#' function). If this process fails at any stage, it will return an error stating
#' that the year is invalid. This process is to repeated for every element in the
#' input vector.
#'
#'@param years A vector of years input by the user, representing the years the
#'  user wishes to get info on.
#'@param year Each element in the vector years.
#'@param file The file name made by the make_filename() function, a character string.
#'@param dat The content from reading file, a dataframe.
#'
#'@return This function will return a list consisting the month and year from the files
#' of given years. No specific print out will the function produce.
#'
#'@importFrom
#'library(magrittr)
#'library(dplyr)
#'fars_read()
#'make_filename()
#'
#'@examples
#'fars_read_years(c(2013,2014,2015))
#'fars_read_years(2005)
#'
#'@export
library(magrittr)
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


#'Summarize data into monthly basis
#'
#'This function will take a vector of years as input. First, it reads through the
#'files of given years and save the months and years data into a list (using \code
#'{fars_read_years()} function). Then it turns the large list into a dataframe
#'(with \code {bind_rows()} function). After that, it groups the dataframe by month
#'and year (using \code{group_by()} function). Then, each group is summarized as
#' the number of elements for each specific month in each year (using \code
#'{summarize()} function). In the end, to tidy up the output, this summary is
#'displayed such that month is the element name and years are the variables spreading
#'out (using \code {spread()} function). Each number represents the number of
#' observations presented in the indicated month of that year.
#'
#'@param dat_list large list, the result of fars_read_years() function
#'
#'@return The function will return a dataframe contains the summary of the files
#'  from the years the user asked for.
#'
#'@importFrom
#'library(magrittr)
#'library(dplyr)
#'fars_read_years()
#'
#'@examples
#'fars_summarize_years(c(2013,2014,2015))
#'fars_summarize_years(2004)
#'
#'@export
library(magrittr)
library(dplyr)


fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}


#'Map accident data of specific year by state
#'
#'This function turns accident data of specified year and state into map. It asks
#'for state number and year from the user as inputs. It first makes the file name
#'(using \code{make_filename()} function) and reads in the data from the file name
#'(using \code{fars_read()} function). Then it ensure that the input state number
#'is an integer (with \code{as.integer()} function). Then the function checks if
#'the input state number is in the data. If it is in the data, the function will
#'proceed to filter the data where only the observations that match for the given
#'state number are selected. After that, the function will check if there are
#'accidents happened in that states. If there are actually accidents, the function
#'will start to map the accidents. First, it rules out any outlier in the selected
#'data (with \code{is.na()} function). After all the cleaning and summarizing data,
#'fhe function finally maps out the accidents as points representing their position
#'in the states (using \code{map()} {points()} functions).
#'
#'@param year the specific year input given by the user
#'@param filename The file name made by the make_filename() function, a character string.
#'@param data The content from reading file, a dataframe.
#'@param state.num state number input given by the user. Latered turned into integer
#'@param data.sub Part of the data where its state variable matches the given state
#'  number
#'@param ylim the limit in the y-direction for the map
#'@param xlim the limit in the x-direction for the map
#'
#'@return The function will return an actual map of the state asked with points on
#'  it representing the position where an accident has happened
#'
#'@importFrom
#'library(magrittr)
#'library(dplyr)
#'library(maps)
#'library(graphics)
#'fars_read()
#'make_filename()
#'
#'@examples
#'fars_map_state(1,2013)
#'fars_map-state(4,2014)
#'fars_map_state(3,2005)
#'
#'@export
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
