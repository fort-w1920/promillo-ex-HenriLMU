#' Estimates your [alcohol level](https://web.archive.org/web/20150123143123/http://promille-rechner.org/erlaeuterung-der-promille-berechnung/)
#'
#' How drunken are you? Calculate your level of drunkenness
#'
#' @param sex character string that is either assignable to male or female
#' @param age number between 10 and 110 measured in years
#' @param height number between 100 and 230 measured in cm
#' @param weight number between 40 and 300 measured in kg
#' @param drinking_time A vector of length 2 containing two posixct, start and end of drinking
#' @param drinks list containing each drink and the amount of drunken glasses as numeric
#' @return blood alcohol level, a numeric measured in thousands
#' @export
tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight,
                              drinking_time, drinks) {
  if (age < 18 && any(names(drinks) == "schnaps")) {
    warning("You might be older than 16 but the hard stuff is still illegal")
  }
  alcohol_drunk <- get_alcohol(drinks)
  bodywater <- get_bodywater(sex, age, height, weight)
  get_permille(alcohol_drunk, bodywater, drinking_time)
}

# utilities --------------------------------------------------------------------

#' calculate drunken mass aclohol from <drinks>
#'
#' The drunken mass of alcohol ist returned in grams and calculated as follows:
#' \deqn{drunken mass= \sum drinks liters alcohol}
#'
#' @inheritParams tell_me_how_drunk
#' @return double of length 1 rendering the total drunken mass of alcohol in gram
#' @import checkmate
get_alcohol <- function(drinks) {
  # homogenize inputs:
  drinks <- unlist(drinks)
  assert_subset(names(drinks), choices = c("massn", "hoibe", "wein", "schnaps"),
    empty.ok = FALSE)
  assert_numeric(drinks, lower = 0)

  volume <- c(
    "massn"    = 1000,
    "hoibe"   = 500,
    "wein"    = 200,
    "schnaps" = 40)
  alcohol_concentration <- c(
    "massn"    = 0.06,
    "hoibe"   = 0.06,
    "wein"    = 0.11,
    "schnaps" = 0.4)
  alcohol_density <- 0.8

  sum(drinks * volume[names(drinks)] *
      alcohol_concentration[names(drinks)] * alcohol_density)
}

#' Calculate a Persons average bodywater
#'
#' Calculate a Persons average bodywater depending on <sex>, <age>, <height> and <weight>
#'
#' @inheritParams tell_me_how_drunk
#' @return estimated Total body water, a numeric measured in liter
get_bodywater <- function(sex = c("male", "female"), age, height, weight) {
  sex <- tolower(sex)
  sex <- match.arg(sex)

  assert_number(age, lower = 10, upper = 110)
  if (age < 16) {
    warning("That's illegal kid")
  }

  assert_number(height, lower = 100, upper = 230)
  assert_number(weight, lower = 40, upper = 300)

  coef <- if (sex == "male") {
    c(2.447, -0.09516, 0.1074, 0.3362)
  } else {
    c(0.203, -0.07, 0.1069, 0.2466)
  }
  t(coef) %*% c(1, age, height, weight)
}

#' Caluculate a Persons alcohol level
#'
#' Caluculate a Persons alcohol level in thousands (permille)
#'
#' @param alcohol_drunk drunken mass aclohol: \code{\link{get_alcohol}}
#' @param bodywater persons average bodywater: \code{\link{get_bodywater}}
#' @inheritParams tell_me_how_drunk
#' @return blood alcohol level, a numeric measured in thousands
get_permille <- function(alcohol_drunk, bodywater, drinking_time){
  assert_posixct(drinking_time, any.missing = FALSE, sorted = TRUE, len = 2)

  alcohol_density <- 0.8
  blood_density <- 1.055
  permille <- alcohol_density * alcohol_drunk / (blood_density * bodywater)

  partylength <- difftime(drinking_time[2], drinking_time[1], units = "hours")
  sober_per_hour <- 0.15
  # sobering up starts only after one hour & you can't be more sober than 0:
  max(0, permille  - (max(0, partylength - 1) * sober_per_hour))
}

