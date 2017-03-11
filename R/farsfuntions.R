#'read the data
#'
#'This is a funtion to read the data you need and set it as a data frame
#'
#'@param data filename you want to analyse
#'@param excited Logical value specifying whether the file exists
#'
#'@return a data frame named "data" or "file \code{data} does not exist"
#'@examples
#'fars_read()
#'
#'@import readr
#'@import dplyr
#'
#'@export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
#'mark the file by years
#'
#'mark the file by different years
#'
#'@param year the year that you want to mark out
#'
#'@return the file named by the year you specified
#'
#'@examples
#'make_filename()
#'
#'@export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
#'read data for years
#'
#'read the data for specific years and save the data to "dat"
#'
#'@param years datas of years that your want to read, returned by \code{\link{make_filename}}
#'@param excites the Logical value specifying whether the file of the year exists
#'
#'@return a data named "dat" or "NULL" and "invalid year: \code{years}" if the \code{years} does not exist
#'
#'@example
#'fars_read_years(2014,2015)
#'
#'@import dplyr
#'
#'@export
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
#'summarize the data of the year
#'
#'summarize number of the data of the year by year and month and make the spread sheet
#'
#'@param years the data of the year you want to summarize,returned by \code{\link{fars_read_years}}
#'
#'@return a data named "dat_list" and a spread sheet
#'
#'@examples
#'fars_summarize_years(2014,2015)
#'
#'@import dplyr
#'@import tidyr
#'
#'@export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
#'draw the map
#'
#'draw the map by year and state number
#'
#'@param state.num the number of the state you want to draw
#'@param the year of the data you want to show
#'
#'@return a map with points
#'@details show a stop \code{"invalid STATE number: state.num"} if the state number is not is the data set. show a message \code{"no accident to plot"} if there is no data for the state in that year.otherwise, a map with points in \code{year}

#'
#'@examples
#'
#'@import dplyr
#'@import maps
#'@import graphics
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
