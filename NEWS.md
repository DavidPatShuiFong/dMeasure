Changelog

# 0.9.15
16th June 2021

## Bugfix

* allow `USERSTATUS` == 3 for `self$db$users`
* empty user list data.frame had 'factorized' strings. change to normal strings

# 0.9.14
4th April 2021

## New

* `db$actions`, fields : `InternalID`, `UserID`, `Added`, `DueDate`, `Performed`, `ActionText`, `Comment`

## Bugfix

* remove references to `CURRENTRX` table, which is no longer accessible in Best Practice (probably since version Saffron)
* remove redundant definitions of `self$db$vaccine_disease`

# 0.9.13
21st November 2020

## Changes

* `self$db$users` now based on `USERS` rather than `BPS_Users` to include 'inactive'
  users.

## Bugfix

* `read_subscripton_db` can now handle (much) more than 700 names in the list
  + not many practices should have that many users, but mine does...
* change call to `dbplyr::in_schema` to opt out of quoting for schemas
  + change required for `dbplyr` version 2.0.0
* `filter_correspondence_named` no longer removes `CheckedBy`, `Notation` and `Action`
* `view_incoming` no longer separates past and future appointments

# 0.9.12
19th August 2020

## New

* add `diabetes_type1_list` and `diabetes_type2_list` methods
* add `glucose_obs` method
* add `README.md`
* `store` option for `$list_contact_*`, `contact_*_list`

## Change

* `db$correspondenceInRaw` name changed to camel-case. string fields trimmed,
  date fields converted with `as.Date`
* replace various `dplyr::filter(x == max(x))` with `dplyr::arrange(desc(x), .by_group = TRUE) %>>% dplyr::filter(dplyr::row_number() == 1)`
  + `arrange` and `slice` breaks ties, where more than one x has the same 'max' value

## Bugfix

* avoid rbinding tibble in `$gluocse_obs`
* `$read_subscription_db` handles situation where no relevant registrations are found

# 0.9.11
8th July 2020

* change license to Mozilla Public License 2.0

## Bugfix

* if Action is NULL, in `filter_investigations` then no investigations will be listed
* if Action is NULL, in `filter_correspondence` then no correspondecne will be listed

# 0.9.10
1st July 2020

## New

* `dMeasureCustom` - custom patient lists
  `initialize_data_table` function read from dMeasureCustom (if available)
   adds table to configuration SQLite database
  `read_configuration_db` function execute from dMeasureCustom (if available)

## Changes

* Changes for new version of DTedit (v2)
* `semantic_tag`/`semantic_button` return empty string if `tag`/`button` is NA
* change `db$currentrx` to `db$currentRx_raw`. add `db$currentRx`
* remove adjust_date from check_subscription
* `check_subscription_datechange_trigR` flip-flops from positive to negative

## Bugfix

* 'quote' contact_maxDate to prevent early evaluation
* `$UserFullConfig` uses only the first row in `$db$practice` to find PracticeName

# 0.9.9
4th May 2020

## New

* `db$patientsRaw` (include HeadOfFamilyID)
* `parent_list` for those listed as head of family

## Change

* increase tolerance of paste2 to unusual Unicode characters

# 0.9.8
29th April 2020

* `dM$contact_maxDate` (and $contact_maxDateR reactive version, and also private version)
* `list_contact_count` includes maxDate (in addition to minDate)
  allows restriction of date of last contact
  e.g. 'has not had contact in past 2 months'

# 0.9.7
25th April 2020

## New

* `db$asthmaplan` and `db$pcehrdocuments`
* `$list_contact_asthma`, `$contact_asthma_list` and `$contact_asthma_listR`
* `$asthmaplan_obs` - list asthma plans
* `$list_asthma_details` - list asthma patients with vaccination and asthma plan status

## Change

* `list_fobt`, `list_cst`, `list_mammogram`
  `include_uptodate` option (default TRUE)
* filter out cancelled and 'reversal' services in `$db$servicesRaw`
* add PayerCode to `$db$servicesRaw`

## Fix

* incorrect labelling of renal disease indication for influenza vaccination

# 0.9.6
1st April 2020

# New

* include_uptodate options for `$list_vax`, `$list_influenza`, `$list_measlesVax`, `$list_zostavax`

# Fix

* correction to `$list_influenza` and `$list_zostavax` tag display logic (erroneously showed 'removed from reminder list')

# 0.9.5
30th March 2020

## New

* add intID/intID_Date options to `$list_zostavax`, `$list_influenza`, `$list_measlesVax`, `$list_vax`

## Change

* change from `$asplenia_list` to `$asplenic_list`
* adjustable `$change_subscription` (add adjust_days)

# 0.9.4
15th March 2020

## New

* add `$db$vaccines` `$db$vaccine_disease` `$db$vaxdiseases` (none of them used, yet)

## Changes

* change from MariaDB to Airtabler
* filter some 'dummy' entries from $list_appointments, such as 'DO NOT BOOK'
* `$read_configuration_db` throws warning and stops if `$config_db` is not a valid,
   even after an attempted opening (by `$read_configuration_db`)
* remove LicenseCheckDate from user configuration

## Fix

* `$server.update` invalid Name logic fixed
* `$userconfig.update` and `$userconfig.delete` checks for `$Fullname` inclusion in description

# 0.9.3
7th March 2020

## Improvements

* faster `$list_appointments` : doesn't retrieve entire patients table!
* reduce calls to `$db$practice` in `$UserFullConfig`
*  strip whitespace from Fullname in `$UserFullConfig`
* $Driver definition in config database Server description (default is "SQL Server")

## Fixes

* remove InternalID '0' patients from `$appointments_filtered`
  - these are 'dummy' appointments
* deal with NA in ProviderNo
* add SENTTOWORKCOVER to `$db$invoices`
   SENTTOWORKCOVER is not a useful field, but for some reason
   some versions of MSSQL odbc client/server configurations require
   the extra field (not all fields work) to maintain non-zero status
   for UserID/InternalID after a select!
* removal of LicenseCheckData from user configuration

## Changes

* subscription change : identifiers are changed to upper case

# 0.9.2
20th February 2020

## Fixes

* `self$UserFullConfig` fix if `self$db$users` empty

# 0.9.1
19th February 2020

## New

* `self$db$Practice` provides access to dbo.PRACTICE
* subscription changes
  + `self$subscription_db` provides access to subscription servers (when open)
  + `self$read_subscription_db` to read subscription information
  + `self$check_subscription` to check subscription and date range
  + can 'trigger' self$check_subscription_datechange_trigR
  + `self$UserConfigLicense` and `$UserConfigLicenseR` - similar to `$UserConfig`, includes `$LicenseDate` and `$Identifier`

## Changes

* `private$.UserRestrictions` does not collect() (delayed evaluation)
* `private$.UserConfig` does not collect() (delayed evaluation)
*  `private$.UserConfig` also does not 'unroll' Locations/Attributes
* `$UserConfig` includes License, LicenseCheckDate
* `private$.identified_user` changed to active (`self$.identified_user`)
*  adopts functionality of match_user function
* improved handling (emptying variables/tables) when databases closed
   or configuration filepath changed

# 0.8.2
24th December 2019

## New

* `list_measlesVax` (measles vaccine eligibility)
* add `self$vaccine_choices` - list of available vaccine lists
* add postnatal_list

## Bugfix

* default 'chosen' in `list_dataQuality` corrected
* correction in `pregnant_list`, allow `is.na(ENDDATE)` for current pregnancy
  improved date logic

# 0.8.1
18th December 2019

## New

* `list_familyHx` (data quality method)
* `db$familyhistory` and `db$familyhistorydetail` : access to family history
  also `db$relationcode` - but this is not actually used at this time

## Bugfix

* convert ResultValue and BPcode to numeric in `db$ReportValues`

  also remove prceding "<" and ">" from ResultValue to allow numerical comparison
  against "<" and ">" values.

  these features require a modified version of dbplyr
  + 'as.double' CASTs to DOUBLE, instead of NUMERIC (the latter casts to integers)
  + use TRY_CAST (a T-SQL/Microsoft SQL specific feature) instead of CAST, to
    allow graceful failure to NA instead of errors.


# 0.8.0
12th November 2019

## New

* dataQuality module - `list_dataQuality`. currently includes `list_allergy`

  Data quality assessment. Are allergies recorded for the listed patients?

  social history (partial) - `list_socialHx`

  list of quality choices in `$dataQuality_choices`

* paste2 - a version of paste which ignores NA, empty strings and character(0)

* addition of `db$reactions`
  addition of KnownAllergies to `db$clinical`

# 0.7.2
28th September 2019

## Bugfixes

* add "Invoice" to appointment_status_types
* fix error if empty `$contact_type` when in `$list_contact_count`

# 0.7.1
27th September 2019

## Bugfix

* process ReportDate to `as.Date()` in observeValues at table level
  failure to convert to as.Date will result in invalid dates being compared

  other date fields in other tables also converted to as.Date() at table level

# 0.7.0
21st September 2019

## Breaking changes

* Moved framingham_riskequation to package framinghamRiskEquation
  at https://github.com/DavidPatShuiFong/framinghamRiskEquation
* Moved billings methods to separate package (module)
  at https://github.com/DavidPatShuiFong/dMeasureBillings
* Moved chronic disease management methods (CDM) to separate package (module)
  at https://github.com/DavidPatShuiFong/dMeasureCDM

# 0.6.0
18th September 2019

## New

* Addition of appointment versions of `list_qim`
  e.g. `list_qim_cvdRisk_appointments`

## Breaking changes

* Move Quality Improvement Measures to separate package (module)
  'dMeasureQIM'

## Improvement

* Framingham risk equation function

  Default is now cardiovascular (instead of coronary heart disease) risk

## Bugfix

* GPMP R/V in CDM had reversed colours (yellow/green)
  and actions ('Overdue'). Fixed

# 0.5.0
9th September 2019

* Added vignette

* Change billings_list method to list_billings
* Integrates appointments, visits and billing contacts into single view
* Add 'own_billings' active field, if FALSE, then all of the patient's
  billings for the day are shown (not just those attributed to the
  same provider listed for that contact)

* Include 'status' in list_appointments return dataframe

* Fixes to server.delete and server.update methods


# 0.4.0
4th September 2019

* Practice Incentive Program (PIP) Quality Improvement Measures
  + lists and reports available for all ten measures

* Mammogram added to cancer screening
* Cervical screening (including Pap) added to cancer screening
