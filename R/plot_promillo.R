#' Vizualizes your level of drunknes during Time
#'
#' Plots your level of drunknes in timeintervalls
#'
#' @inheritParams tell_me_how_drunk
#' @param distance determine the distance in minutes between each new measurement; default is set to 5 minutes
#' @return a ggplot-object showing the course of your alcohol intoxication
#' @seealso [tell_me_how_drunk()] for estimation of alcohol level
#' @import ggplot2
#' @export
show_me_how_drunk <- function(age, sex, height, weight, drinking_time, drinks, distance = 5) {
  # Get alcohol drunken in grams and bodywater in liters
  alcohol_drunk <- get_alcohol(drinks)
  bodywater <- get_bodywater(sex, age, height, weight)

  alcohol_course <- list_alcohol_course(alcohol_drunk, bodywater, drinking_time, distance)

  qplot(as.POSIXct(names(alcohol_course)), alcohol_course,
        xlab = "time", ylab = "alcohol level")

}

# utilities --------------------------------------------------------------------
#' Lists your level of drunknes during Time
#'
#' Prepares a vector of alcohol levels in an intervall of time for plotting with [show_me_how_drunk()]
#'
#' @inheritParams show_me_how_drunk
#' @inheritParams get_permille
#' @return a vector containing time and alcohollevel
list_alcohol_course <- function(alcohol_drunk, bodywater, drinking_time, distance){

  length <- ceiling(difftime(drinking_time[2],
                     drinking_time[1],
                     units = "mins") / distance + 1)
  alcohol_course <- vector(mode = "numeric",
                          length = length)
  time_interval <- c(drinking_time[1], drinking_time[1])
  i <- 1


  while (i <= length(alcohol_course)) {
    names(alcohol_course)[i] <- as.character(time_interval[2])
    alcohol_course[i] <- get_permille(alcohol_drunk, bodywater, time_interval)
    time_interval[2] <- time_interval[2] + distance * 60
    i <- i + 1
  }
  alcohol_course
}

