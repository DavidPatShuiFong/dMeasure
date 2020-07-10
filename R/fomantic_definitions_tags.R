# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

# fomantic (semantic.ui) string functions
# only the tag definitions, not functions required to show the 'styled' DT datatable

#' Create semantic/fomantic tags with attached tooltips (text and HTML)
#'
#' User-defined colour and popuptext (tooltip) or popuphtml (HTMl tooltip)
#'
#' @param tag list of tag contents
#' @param colour list of colours
#' @param popuptext list of popuptexts
#' @param popuphtml (alternative) list of popup html
#'
#' @return vector of semantic/fomantic tags
#'
#' @export
semantic_tag <- function(tag, colour = "", popuptext = NA, popuphtml = NA) {
  #
  tags <- paste0(
    '<span class="huge ', colour, ' ui tag label"',
    ifelse(
      !is.na(popuphtml),
      paste0('data-variation="wide" data-position = "left center"',
             'data-htmltagX="', # this is a 'dummy' attribute
             # to help datatables order this column alphabetically!
             tag, # will order this column alphabetically by 'tag'
             '"',

             'data-html="',
             popuphtml,
             '"',
             sep = ""
      ),
      # 'data-variation' is only available in the
      # fomantic version of semantic.ui
      # as of writing, semantic.ui does not allow
      # variation in text-size of javascript-free tag
      ""
    ),
    "> ",
    ifelse(
      !is.na(popuptext),
      paste0('<span data-tooltip = "',
             popuptext,
             '" data-variation = "wide huge" data-position = "left center">',
             sep = ""
      ),
      ""
    ),
    tag,
    ifelse(!is.na(popuptext), "</span>", ""),
    " </span>",
    sep = ""
  )
  tags[is.na(tag)] <- "" # if tag is NA, then return empty string ""
  return(tags)
  # paste0 is vectorized version of 'paste'
}

#' Create semantic/fomantic buttons with attached tooltips (text and HTML)
#'
#' user-defined colour and popuptext (tooltip) or popuphtml (HTML tooltip)
#'
#' @param button list of buttons contents
#' @param colour list of colours
#' @param popuptext list of popup texts
#' @param popuphtml (alternative) list of popup html
#'
#' @return vector of semantic/fomantic buttons
#'
#' @export
semantic_button <- function(button, colour = "", popuptext = NA, popuphtml = NA) {

  buttons <- paste0(
    '<span class="huge ', colour, ' ui button"',
    ifelse(
      !is.na(popuphtml),
      paste0('data-variation="wide" data-position = "left center"',
             'data-htmltagX="', # this is a 'dummy' attribute
             # to help datatables order this column alphabetically!
             button, # will order this column alphabetically by 'button'
             '"',
             'data-html="',
             popuphtml,
             '"',
             sep = ""
      ),
      # 'data-variation' is only available
      # in the fomantic version of semantic.ui
      # as of writing, semantic.ui does not allow variation
      # in text-size of javascript-free tags
      ""
    ),
    "> ",
    ifelse(
      !is.na(popuptext),
      paste0('<span data-tooltip = "',
             popuptext,
             '" data-variation = "wide huge" data-position = "left center">',
             sep = ""
      ),
      ""
    ),
    button,
    ifelse(!is.na(popuptext), "</span>", ""),
    " </span>",
    sep = ""
  ) # paste0 is vectorized version of 'paste'
  buttons[is.na(button)] <- "" # if tag is NA, then return empty string ""
  return(buttons)
}
