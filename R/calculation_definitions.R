#' @include dMeasure.R
#' requires .public
NULL

## 'helper' functions for calculation

#' Calculate age a given reference date
#'
#' Create an interval between the date of birth and the enrollment date;
#'
#' @param birthdate vector of dates
#' @param refDate reference date(s)
#'
#' @return period(s) (in years)
#' @export
calc_age <- function(birthDate, refDate = Sys.Date()) {
  # Calculate age at a given reference date

  if (length(birthDate) == 0) {return(numeric(0))}
  # empty vector, so return empty numeric

  period <- mapply(function(x, y)
    # Arguments can be vectors, so need to use mapply
    (ifelse(is.na(x) | x == -Inf, NA,
            length(seq.Date(min(x, y), max(x, y), by = "year")) - 1 ) *
       ifelse(y > x, 1, -1)),
    # note that seq.Date can't handle 'negative' periods
    birthDate, refDate)

  period <- as.numeric(period)
  # if not converted, could return an empty list, instead of empty numeric

  return(period)
}

#' Add age to a given reference date
#'
#' Adds an interval (years) to a birthDate
#'
#' @param birthdate vector of dates
#' @param age numeric
#' @param by default is "year", but can be, for example "-1 month"
#'
#' @return vector of dates
#' @export
add_age <- function(birthDate, age, by = "year") {
  # Calculate age at a given reference date

  if (length(birthDate) == 0) {return(birthDate)}
  # empty vector, so return empty vector

  dates <- as.Date(mapply(function(x, y)
    # Arguments can be vectors, so need to use mapply
  {ifelse(is.na(x) | x == -Inf, as.Date(NA),
          tail(seq(from = x, by = by, length.out = y), 1))},
  birthDate, age + 1), origin = "1970-01-01")

  dates <- as.Date(dates, origin = "1970-01-01")
  # if not converted, could return an empty list, instead of empty dates

  return(dates)
}

#' Calculate age a given reference date, in months
#'
#' Create an interval between the date of birth and the enrollment date;
#'
#' @param birthdate vector of dates
#' @param refDate reference date(s)
#'
#' @return period(s) (in months)
#' @export
calc_age_months <- function(birthDate, refDate = Sys.Date()) {
  # Create an interval between the date of birth and the enrollment date;
  # note that arguments can be vectors, so need to use mapply

  if (length(birthDate) == 0) {return(numeric(0))}
  # empty vector, so return empty numeric

  period <- mapply(function(x, y)
    (ifelse(is.na(x) | x == -Inf, NA,
            length(seq.Date(min(x, y), max(x, y), by = "month")) - 1) *
       ifelse(y > x, 1, -1)),
    # note that seq.Date can't handle 'negative' periods
    birthDate, refDate)

  period <- as.numeric(period)
  # if not converted, could return an empty list, instead of empty numeric

  return(period)
}

#' Calculate period between two dates
#'
#' Create an interval between the date_a and date_b
#'
#' can return 'negative' numbers
#' returns NA if either of date_a or date_b is NA
#' returns an arbitrarily large number of years (Inf) if min(date_a, date_b) is -Inf
#
#' @param date_a vector of dates
#' @param date_b vector of dates
#' @param unit "none" or "month". if "month", convert all years tomonths
#'
#' @return period(s) in $year, $month and $day
#' @export
interval <- function(date_a, date_b, unit = "none") {

  infinity_years <- Inf

  interval <- list(year = numeric(0), month = numeric(0), day = numeric(0))

  if (length(date_a) == 0 || length(date_b) == 0) {return(interval)}
  # empty input vector, so return list of empty vectors

  interval$year <- mapply(function(x, y)
    ifelse(!is.na(min(x, y)),
           ifelse(min(x,y) == -Inf,
                  infinity_years,
                  (length(seq.Date(min(x, y), max(x, y), by = "year")) - 1) *
                    ifelse(y > x, 1, -1)),
           NA),
    # note that seq.Date can't handle 'negative' periods
    date_a, date_b)
  interval$year <- as.numeric(interval$year) # if empty, converts from empty list to numeric(0)

  interval$month <- mapply(function(x, y, z)
    ifelse(!is.na(min(x, y)),
           (ifelse(min(x,y) == -Inf,
                   0,
                   length(seq.Date(tail(seq.Date(min(x, y), length.out = abs(z) + 1,
                                                 by = "year"), 1),
                                   # 'reduces' difference between dates by 'year' difference
                                   max(x,y), by = "month")) -1 ) *
              ifelse(y > x, 1, -1)),
           NA),
    date_a, date_b, interval$year)
  interval$month <- as.numeric(interval$month)

  interval$day <- mapply(function(x, y, z, zz)
    ifelse(!is.na(min(x, y)),
           (ifelse(min(x,y) == -Inf,
                   0,
                   length(seq.Date(tail(seq.Date(tail(seq.Date(min(x, y),
                                                               length.out = abs(z) + 1,
                                                               by = "year"), 1),
                                                 length.out = abs(zz) + 1, by = "month"), 1),
                                   # 'reduces' difference between dates by 'year' difference
                                   max(x,y), by = "day")) -1 ) *
              ifelse(y > x, 1, -1)),
           NA),
    date_a, date_b, interval$year, interval$month)
  interval$day <- as.numeric(interval$day)

  if (unit == "month" & length(date_a)>0) {
    interval$month <- interval$month + interval$year*12
    interval$year <- replicate(length(interval$month), 0)
  }

  return(interval)
}

#' Calculate seconds to a 'time' starting from midnight
#'
#' t: value in seconds
#'
#' @param t seconds
#'
#' @return 24-hour time of form '14:15' (hh:mm)
#' @export
hrmin <- function(t) {

  format(as.POSIXct('1900-1-1') + t, '%H:%M')
}

# code for encoding/decoding. not 'very' secret
# requires libraries jsonlite (provides base64enc partly for obfuscation)
# and sodium

#' Simple encoder
#'
#' Simple encode of text strings, will output a text string.
#' Uses sodium library and base64_enc/dec from jsonlite. Has some defaults, but
#' will also take command-line arguments or read from environment
#'
#' @param msg the text to encode
#' @param key the cipher, which can be set manually, otherwise will read from env
#' @param nonce a non-secret unique data value used to randomize the cipher
#'
#' @return - the encrypted text.
#'
#'   returns NA for elements of msg which are NA
#'
#' @export
simple_encode <- function(msg, key = NULL, nonce = NULL) {
  if (is.null(nonce)) {
    # non-secret unique data 'nonce' used to randomize the cipher
    nonce <- sodium::hex2bin(paste0("89:63:73:bc:dc:eb:98:14:59:ce:17:4f:",
                                    "6e:0a:75:15:83:0c:36:00:f2:6e:f7:07"))
    # the 24 bytes of hexadecimal digits created by paste0(random(24), collapse = ":")
  }
  if (is.null(key)) {
    if (nchar(Sys.getenv("DailyMeasure_Value2"))>0) {
      # if not set then the number of characters will be zero
      key <- Sys.getenv("DailyMeasure_value2")
      # this can be set in .Renviron
    } else {
      key <- "noncenonce"
    }
  }
  key <- sodium::hash(charToRaw(key))

  return(vapply(msg,
                function(n) {
                  if (is.na(n)) {
                    as.character(NA)}
                  # can't encode a 'NA' (that causes an error)
                  else {
                    jsonlite::base64_enc(
                      sodium::data_encrypt(charToRaw(n), key, nonce))}},
                FUN.VALUE = character(1),
                USE.NAMES = FALSE))
}

#' Simple decoder
#'
#' Simple decoder of text strings, will output a text string.
#' Uses sodium library and base64_enc/dec from jsonlite. Has some defaults, but
#' will also take command-line arguments or read from environment.
#' Companion function to simple_encode
#'
#' @param msg the text to decode
#' @param key the cipher, which can be set manually, otherwise will read from env
#' @param nonce a non-secret unique data value used to randomize the cipher
#'
#' @return - the encrypted text
#'
#'   returns NA for elements of msg which are NA, or "" empty string.
#'   note that simple_encode will ENCRYPT an empty string "".
#'
#' @export
simple_decode <- function(msg, key = NULL, nonce = NULL) {
  if (is.null(nonce)) {
    # non-secret unique data 'nonce' used to randomize the cipher
    nonce <- sodium::hex2bin(paste0("89:63:73:bc:dc:eb:98:14:59:ce:17:4f:",
                                    "6e:0a:75:15:83:0c:36:00:f2:6e:f7:07"))
    # the 24 bytes of hexadecimal digits created by paste0(random(24), collapse = ":")
  }
  if (is.null(key)) {
    if (nchar(Sys.getenv("DailyMeasure_Value2"))>0) {
      # if not set then the number of characters will be zero
      key <- Sys.getenv("DailyMeasure_value2")
      # this can be set in .Renviron
      # or with Sys.setenv(DailyMeasure_value2="password")
    } else {
      key <- "noncenonce"
    }
  }
  key <- sodium::hash(charToRaw(key))

  return(vapply(msg,
                function(n) {
                  if (is.na(n) || n == "") {
                    as.character(NA)}
                  # can't decode a 'NA' (that causes an error)
                  else {
                    rawToChar(sodium::data_decrypt(
                      jsonlite::base64_dec(n),key, nonce))}},
                FUN.VALUE = character(1),
                USE.NAMES = FALSE))
}

#' Simple tagger
#'
#' Simple tagger of text strings, will output a text string.
#' Uses sodium library and base64enc. Has some defaults, but
#' will also take command-line arguments or read from environment.
#'
#' @param msg the text to decode
#' @param key the cipher, which can be set manually, otherwise will read from env
#'
#' @return - the hash
#' @export
simple_tag <- function(msg, key = NULL) {
  if (is.null(key)) {
    if (nchar(Sys.getenv("DailyMeasure_Value3"))>0) {
      # if not set then the number of characters will be zero
      key <- Sys.getenv("DailyMeasure_value3")
      # this can be set in .Renviron
      # or with Sys.setenv(DailyMeasure_value2="password")
    } else {
      key <- "noncenonce"
    }
  }
  key <- sodium::hash(charToRaw(key))
  msg <- serialize(msg, NULL)
  tag <- jsonlite::base64_enc(sodium::data_tag(msg, key))

  return(tag)
}

#' Simple tag comparison
#'
#' Simple tagger of text strings, will output a text string.
#' Uses sodium library and base64enc. Has some defaults, but
#' will also take command-line arguments or read from environment.
#'
#' @param msg the text to check
#' @param tag the tagged message (base64 encoded)
#' @param key the cipher, which can be set manually, otherwise will read from env
#'
#' @return - TRUE if matching, FALSE otherwise
#' @export
simple_tag_compare <- function(msg, tag, key = NULL) {
  if (is.null(key)) {
    if (nchar(Sys.getenv("DailyMeasure_Value3"))>0) {
      # if not set then the number of characters will be zero
      key <- Sys.getenv("DailyMeasure_value3")
      # this can be set in .Renviron
      # or with Sys.setenv(DailyMeasure_value2="password")
    } else {
      key <- "noncenonce"
    }
  }
  key <- sodium::hash(charToRaw(key))
  msg <- serialize(msg, NULL)
  newtag <- sodium::data_tag(msg, key)
  oldtag <- jsonlite::base64_dec(tag)

  result <- all(ifelse(newtag == oldtag, TRUE, FALSE))
  # ifelse is vectorized, and will return a vector of TRUE/FALSE
  # 'all' checks that that all the elements of the comparison vector are TRUE

  return(result)
}

#' paste which can ignore NA and empty strings
#'
#' Acts the same as regular paste, unless na.rm = TRUE,
#' in which case empty strings and NA are removed
#'
#' based on code by Moody_Mudskipper at
#' https://stackoverflow.com/questions/13673894/suppress-nas-in-paste
#' with additional code from
#' https://stackoverflow.com/questions/14270950/suppress-separator-in-paste-when-values-are-missing
#'
#' @param ... the list of strings to paste
#' @param sep the separator string, " " by default
#' @param collapse the collapse string, NULL by default
#' @param na.rm whether to remove NA and empty strings
#'
#' @return string
#' @export
paste2 <- function(..., sep = " ", collapse = NULL, na.rm = FALSE){
  # in default case, use paste
  if(!na.rm) return(paste(..., sep = sep, collapse = collapse))
  # cbind is convenient to recycle, it warns though so use suppressWarnings
  dots <- suppressWarnings(cbind(...))
  res <- apply(dots, 1, function(...) {
    x <- c(...)
    x <- x[nchar(x) > 0] # get rid of empty strings
    x <- x[length(x) > 0] # get rid of character(0)
    if (all(is.na(x))) return(c(""))
    do.call(paste, as.list(c(na.omit(x), sep = sep)))
  })
  if(is.null(collapse)) res else
    paste(na.omit(res), collapse = collapse)
}
