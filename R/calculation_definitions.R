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
#' Calculate cardiovascular disease risk according to Framingham Risk Equation
#'
#' @details
#'
#' sourced from National Vascular Disease Prevention Alliance (Australia)
#'
#' http://cvdcheck.org.au/pdf/Absolute_CVD_Risk-Quick_Reference_Guide.pdf
#'
#' "An Updated Coronary Risk Profile - A Statement for Health Professionals"
#' Keaven M. Anderson, Peter W.F. Wilson, Patricia M. Odell, William B. Kannel
#'
#' AHA (American Heart Association) Medical/Scientific Statement
#'
#' sourced from http://ahajournals.org
#'
#' and "Cardiovascular Disease Risk Profiles"
#' Keaven M. Anderson, Patricia M. Odell, Peter W.F. Wilson, William B. Kannel
#' American Heart Journal : 1991; 121:293-298
#'
#' @param df a dataframe, which must include the following columns/fields...
#'
#'  InternalID (numeric)
#'
#'  CardiovascularDisease (logical)
#'
#'  Diabetes (logical)
#'
#'  SmokingDate (date) - not actually used in this equation
#'
#'  SmokingStatus (character) - "Smoker" if a smoker. All other values ignored
#'
#'  UrineAlbuminDate (character), UrineAlbuminValue (double), UrineAlbuminUnit (character)
#'
#'  PersistentProteinuria (logical)
#'
#'  eGFRDate (date), eGFRValue (double), eGFRUnits (character)
#'
#'  FamilialHypercholesterolaemia (logical)
#'
#'  LVH (logical) = left ventricular hyp0ertrophy
#'
#'  CholesterolDate (date), Cholesterol (double), HDL (double), LDL (double),
#'   Triglycerides (double), CholHDLRatio (double)
#'
#'  BPDate (date), BP (character, two numbers separated by "/")
#'
#'  Sex (character), Ethnicity (character)
#'
#'  Age (double)
#'
#' @param years number of years to predict (from 4 to 12). default is 5 years.
#' @param outcome (default is "CVD") "CHD" - coronary heart disease. includes myocardial
#'  infarction, death from coronary heart disease plus angina pectoris and
#'  coronary insufficiency.
#'
#'  "CVD" cardiovascular disease (the default) includes coronary heart disease,
#'  stroke (including transient ischaemia), congestive heart failure and
#'  peripheral vascular disease.
#'
#' @return dataframe
#'
#'  InternalID
#'
#'  frisk : numeric (a number, or 'NA' if not enough information to computer)
#'
#'  friskHI : either 'NA' or '>15\%'
#'
#'  '>15\%' are groups considered equivalent risk to already having IHD
#'
#' @examples
#'
#' framingham_riskequation(data.frame(InternalID = 1, BP = "135/80", Sex = "Female",
#'  Age = 55,
#'  SmokingStatus = "Smoker", CholHDLRatio = 230/48, Diabetes = TRUE, LVH = FALSE,
#'   CardiovascularDisease = FALSE, PersistentProteinuria = FALSE, eGFRValue = NA,
#'    eGFRUnits = NA, UrineAlbuminValue = NA, UrineAlbuminUnits = NA,
#'    FamilialHypercholesterolaemia = NA, Cholesterol = 5.96, Ethnicity = NA),
#'     outcome = "CHD", years = 10)
#'     # this comes from "Cardiovascular disease risk profiles" (Anderson 1991)
#'     # the worked answer in the paper is 0.22. this function returns 0.2189125
#'     # the same risk-factors with outcome = "CVD" and years = 5 returns 0.180
#'     #  (cvdcheck.org.au reports 18%)
#'     # the same risk-factors except Sex = "Male" with outcome = "CVD" and years = 5 returns 0.202
#'     #  (cvdcheck.org.au reports 20%)
#'
#' framingham_riskequation(data.frame(InternalID = 2, BP = "130/80", Sex = "Male",
#'  Age = 55,
#'  SmokingStatus = "Smoker", CholHDLRatio = 240/45, Diabetes = FALSE, LVH = FALSE,
#'   CardiovascularDisease = FALSE, PersistentProteinuria = FALSE, eGFRValue = NA,
#'    eGFRUnits = NA, UrineAlbuminValue = NA, UrineAlbuminUnits = NA,
#'    FamilialHypercholesterolaemia = NA, Cholesterol = 6.22, Ethnicity = NA),
#'     outcome = "CHD", years = 10)
#'     # this comes from "An Updated Coronary Risk Profile" (Anderson 1991)
#'     # the worked answer in the paper is 0.192, this function returns 0.1919
#'     # the same risk-factors with outcome = "CVD" and years = 5 returns 0.133
#'     #  (cvdcheck.org.au reports 13%)
#'     # the same risk-factors except LVH = TRUE (outcome = CVD, =years 5) returns 0.211
#'     #  (cvdcheck.org.au reports 21%)
#'
#' framingham_riskequation(data.frame(InternalID = 3, BP = "130/80", Sex = "Female",
#'  Age = 55,
#'  SmokingStatus = "Smoker", CholHDLRatio = 240/45, Diabetes = FALSE, LVH = FALSE,
#'   CardiovascularDisease = FALSE, PersistentProteinuria = FALSE, eGFRValue = NA,
#'    eGFRUnits = NA, UrineAlbuminValue = NA, UrineAlbuminUnits = NA,
#'    FamilialHypercholesterolaemia = NA, Cholesterol = 6.22, Ethnicity = NA),
#'     outcome = "CHD", years = 10)
#'     # this comes from "An Updated Coronary Risk Profile" (Anderson 1991)
#'     # the worked answer in the paper is 0.135, this function returns 0.1349
#'     # the same risk-factors with outcome = "CVD" and years = 5 returns 0.088
#'     #  (cvdcheck.org.au reports 9%)
#'     # the same risk-factors except LVH = TRUE (outcome = CVD, =years 5) returns 0.150
#'     #  (cvdcheck.org.au reports 15%)
#'
#' @export
framingham_riskequation <- function(df, years = 5, outcome= "CVD") {

  fre_coefficients <- data.frame(row.names = c("CHD", "CVD"),
                                 theta0 = c(0.9145, 0.6536),
                                 theta1 = c(-0.2784, -0.2402),
                                 beta0 = c(15.5305, 18.8144),
                                 female = c(28.4441, -1.2146),
                                 log_age = c(-1.4792, -1.8443),
                                 log_age2 = c(0, 0),
                                 log_age_female = c(-14.4588, 0.3668),
                                 log_age2_female = c(1.8515, 0),
                                 log_SBP = c(-0.9119, -1.4032),
                                 cigarettes = c(-0.2767, -0.3899),
                                 log_TCHDLratio = c(-0.7181, -0.5390),
                                 diabetes = c(-0.1759, -0.3036),
                                 diabetes_female = c(-0.1999, -0.1697),
                                 ECGLVH = c(-0.5865, -0.3362))

  fre_b <- fre_coefficients[outcome,]

  f <- df %>>%
    tidyr::separate(BP, into = c("Systolic", "Diastolic"), sep = "/", convert = TRUE) %>>%
    # creates new Systolic and Diastolic fields,
    # fills with NA if not available, and converts to numeric
    dplyr::mutate(mu = fre_b$beta0 +
                    fre_b$female * (Sex == "Female") +
                    fre_b$log_age * log(Age) +
                    fre_b$log_age_female * log(Age) * (Sex == "Female") +
                    fre_b$log_age2_female * log(Age)^2 * (Sex == "Female") +
                    fre_b$log_SBP * log(Systolic) +
                    fre_b$cigarettes * (SmokingStatus == "Smoker") +
                    fre_b$log_TCHDLratio * log(CholHDLRatio) +
                    fre_b$diabetes * Diabetes +
                    fre_b$diabetes_female * Diabetes * (Sex == "Female") +
                    fre_b$ECGLVH * LVH,
                  sigma = exp(fre_b$theta0 + fre_b$theta1 * mu),
                  u = (log(years) - mu)/sigma,
                  frisk = 1 - exp(-exp(u))) %>>%
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
      Ethnicity %in% c("Aboriginal", "Torres Strait Islander",
                       "Aboriginal/Torres Strait Islander") &
        Age > 74 ~ ">15%",
      TRUE ~ as.character(NA))) %>>%
    dplyr::select(InternalID, frisk, friskHI)

  return(f)
}
