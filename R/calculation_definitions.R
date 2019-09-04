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
#' @return - the encrypted text
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
  return(jsonlite::base64_enc(
    sodium::data_encrypt(charToRaw(msg), key, nonce)))
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
  return(rawToChar(sodium::data_decrypt(
    jsonlite::base64_dec(msg),key, nonce)
  ))
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

#' Framingham Risk Equation
#'
#' sourced from National Vascular Disease Prevention Alliance (Australia)
#' http://cvdcheck.org.au/pdf/Absolute_CVD_Risk-Quick_Reference_Guide.pdf
#'
#' and from "An Updated Coronary Risk Profile - A Statement for Health Professionals"
#' Keaven M. Anderson, Peter W.F. Wilson, Patricia M. Odell, William B. Kannel
#'
#' AHA (American Heart Association) Medical/Scientific Statment
#' sourced from http://ahajournals.org
#'
#' @param df a dataframe
#'  Patient (chr = character)
#'  InternalID (numeric)
#'  CardiovascularDisease (logical)
#'  Diabetes (logical)
#'  SmokingDate (date), SmokingStatus (chr)
#'  UrineAlbuminDate (chr), UrineAlbuminValue (double), UrineAlbuminUnit (chr)
#'  PersistentProteinuria (logical)
#'  eGFRDate (date), eGFRValue (double), eGFRUnits (chr)
#'  FamilialHypercholesterolaemia (logical)
#'  LVH (logical) = left ventricular hyp0ertrophy
#'  CholesterolDate (date), Cholesterol (double), HDL (double), LDL (double),
#'   Triglycerides (double), CholHDLRatio (double)
#'  BPDate (date), BP (character, two numbers separated by "/")
#'  Sex (character), Ethnicity (character)
#'  RecordNo (character), MaritalStatus (character), Sexuality (character)
#'  DOB (character), Age (double)
#' @param years number of years to predict (from 4 to 12)
#'
#' @return dataframe
#'  InternalID
#'  frisk : numeric
#'  friskHI : either 'NA' or '>15%'
#'    '>15%' are groups considered equivalent risk to already having IHD
#'
#' @export
framingham_riskequation <- function(df, years = 5) {
  f <- df %>>%
    tidyr::separate(BP, into = c("Systolic", "Diastolic"), sep = "/", convert = TRUE) %>>%
    # creates new Systolic and Diastolic fields, fills with NA if not available, and converts to numeric
    dplyr::mutate(a = 11.1122 - 0.9119 * log(Systolic) - 0.2767 * (SmokingStatus == "Smoker")
                  -0.7181 * log(CholHDLRatio) - 0.5865 * LVH,
                  m = dplyr::if_else(Sex == "Female",
                                     a - 5.8549 + 1.8515 * (log(Age/74))**2 - 0.3758 * Diabetes,
                                     a - 1.4792 * log(Age) - 0.1759 * Diabetes),
                  mu = 4.4181 + m,
                  sigma = exp(-0.3155 - 0.2784 * m),
                  u = (log(years) - mu)/sigma,
                  # 5 is the predicted number years
                  frisk = 1 - exp (-exp(u))) %>>%
    dplyr::mutate(friskHI = dplyr::case_when(
      CardiovascularDisease ~ ">15%",
      Diabetes & (Age > 60) ~ ">15%",
      Diabetes & UrineAlbuminValue > 20 & UrineAlbuminUnits == "mcg/min" ~ ">15%",
      Diabetes & Sex == "Male" &
        UrineAlbuminValue > 2.5 & UrineAlbuminUnits == "mg/mmol" ~ ">15%",
      Diabetes & Sex == "Female" &
        UrineAlbuminValue > 3.5 & UrineAlbuminUnits == "mg/mmol" ~ ">15%",
      PersistentProteinuria == TRUE ~ ">15%",
      eGFRValue < 45 & eGFRUnits == "mL/min" ~ ">15%",
      FamilialHypercholesterolaemia == TRUE ~ ">15%",
      Systolic >= 180 | Diastolic >= 110 ~ '>15%',
      Cholesterol > 7.5 ~ ">15%",
      Ethnicity %in% c("Aboriginal", "Torres Strait Islander", "Aboriginal/Torres Strait Islander") &
        Age > 74 ~ ">15%",
      TRUE ~ as.character(NA))) %>>%
    dplyr::select(InternalID, frisk, friskHI)
}
