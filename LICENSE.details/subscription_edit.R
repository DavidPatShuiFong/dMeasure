library(shiny)
library(airtabler)
library(DTedit)
library(magrittr)
library(dplyr)
library(dMeasure)
library(stringi)

ui <- fluidPage(
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "Subscriptions",
      h3("Subscriptions"),
      dteditmodUI("subscriptions")
    ),
    tabPanel(
      "Bulk import",
      h3("Bulk import"),
      fileInput(
        "file",
        "Choose CSV file",
        accept = c(
          "text/csv",
          "text/comma-separated-values, text/plain",
          ".csv"
        )
      ),
      tags$hr(),
      checkboxInput("header", "Header", TRUE)
    ),
    tabPanel(
      "Keys",
      h3("Keys"),
      passwordInput("api_key", "API Key", value = ""),
      passwordInput("subscription_key", "subscription key", value = "")
    )
  )
)


server <- function(input, output) {
  subscriptions <- reactiveVal() # later will point to database

  table_base <- reactiveVal(data.frame(
    id = character(),
    Key = character(), License = character(),
    Comment = character(),
    stringsAsFactors = FALSE
  ))

  table_edit <- reactiveVal(data.frame(
    id = character(),
    Key = character(), License = character(),
    Comment = character(),
    Name = character(),
    Date = as.Date(numeric(0), origin = "1970-01-01"),
    stringsAsFactors = FALSE
  ))

  observeEvent(input$api_key, ignoreNULL = TRUE, ignoreInit = TRUE, {
    Sys.setenv("AIRTABLE_API_KEY" = input$api_key)
    subscriptions(airtabler::airtable("appLa2AH6S1SUCxE3", "Subscriptions"))
    # note that this fails very ungracefully if not a valid API_KEY
    table_base(subscriptions()$Subscriptions$select() %>% dplyr::select(c("id", "Key", "License", "Comment")))
  })

  bulk_csv <- eventReactive(input$file, ignoreNULL = TRUE, ignoreInit = TRUE, {
    inFile <- input$file
    x <- read.csv(
      inFile$datapath,
      header = input$header,
      stringsAsFactors = FALSE
    )
    x$Identifier <- toupper(trimws(x$Identifier))
    x$Key <- dMeasure::simple_encode(
      x$Identifier,
      key = input$subscription_key
    )
    x
  })

  observeEvent(bulk_csv(), ignoreNULL = TRUE, {
    showModal(
      modalDialog(
        dateInput(
          "bulk_date",
          "Subscription end date",
          value = Sys.Date() + 365,
          min = Sys.Date() + 30,
          max = Sys.Date() + 750
        ),
        br(),
        textInput(
          "bulk_comment",
          "Comment",
          value = "",
          placeholder = "General comment (could be prefix)"
        ),
        br(),
        checkboxInput(
          "bulk_name_postfix",
          "Add name to comment (postfix)",
          FALSE
        ),
        title = "Subscription details",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("bulk_ok", "OK")
        )
      )
    )
  })

  observeEvent(input$bulk_ok, ignoreNULL = TRUE, ignoreInit = TRUE, {
    x <- bulk_csv()
    if (input$bulk_name_postfix) {
      x$Comment <- paste(input$bulk_comment, x$Fullname)
      # adds name to 'general' comment
    } else {
      x$Comment <- input$bulk_comment
      # just the 'general' comment
    }
    x$License <- dMeasure::simple_encode(
      paste0(
        x$Identifier,
        as.character(input$bulk_date)
      ),
      key = input$subscription_key
    )
    for (i in 1:nrow(x)) {
      # iterate through the dataframe
      y <- x[i, ]
      record_data <- list(
        Key = y$Key,
        License = y$License,
        Comment = y$Comment
      )
      new_df_row <- data.frame(
        Key = y$Key,
        License = y$License,
        Comment = y$Comment,
        Name = y$Identifier,
        Date = as.Date(input$bulk_date),
        id = "" # dummy. fill it in later
      )
      table_edit_index <- which(table_edit()$Key == y$Key) # match keys
      if (length(table_edit_index) == 0) {
        # a new entry
        new_row <- subscriptions()$Subscriptions$insert(record_data)
        new_df_row$id = new_row$id # now we know the ID
        table_edit(rbind(table_edit(), new_df_row))
        # add the new row to the dataframe
      } else {
        # update entry
        new_df_row$id = table_edit()[table_edit_index, ]$id
        # we already know the ID
        subscriptions()$Subscriptions$update(
          record_id = table_edit()[table_edit_index, ]$id,
          record_data = record_data
        )
        new_df <- table_edit() %>%
          anti_join(new_df_row, by = "id") %>%
          # remove the duplicate row
          # before adding new_df_row back in again
          bind_rows(new_df_row)
        table_edit(new_df)
      }
    }
    removeModal()
  })

  observeEvent(
    input$subscription_key,
    ignoreNULL = TRUE, ignoreInit = TRUE, {
      dummy <- table_base()
      dummy$Name <- table_base() %>%
        pull(Key) %>%
        dMeasure::simple_decode(key = input$subscription_key)
      dummy$Date <- table_base() %>%
        pull(License) %>%
        dMeasure::simple_decode(key = input$subscription_key) %>%
        stringi::stri_sub(-10, -1) %>% # doesn't formally check the validity of the License Key
        as.Date()
      table_edit(dummy)
    }
  )

  table.insert.callback <- function(data, row) {
    data[row, ]$Name <- toupper(trimws(data[row, ]$Name))
    data[row, ]$Key <- dMeasure::simple_encode(
      data[row, ]$Name,
      key = input$subscription_key
    )
    data[row, ]$License <- dMeasure::simple_encode(
      paste0(
        data[row, ]$Name,
        as.character(data[row, ]$Date)
      ),
      key = input$subscription_key
    )
    record_data <- list(
      Key = data[row, ]$Key,
      License = data[row, ]$License,
      Comment = data[row, ]$Comment
    )
    new_row <- subscriptions()$Subscriptions$insert(record_data)
    data[row, ]$id <- new_row$id
    return(data)
  }

  table.delete.callback <- function(data, row) {
    subscriptions()$Subscriptions$delete(data[row, ]$id)
    return(data[-c(row), ])
  }

  table.update.callback <- function(data, olddata, row) {
    data[row, ]$Name <- toupper(trimws(data[row, ]$Name))
    data[row, ]$Key <- dMeasure::simple_encode(
      data[row, ]$Name,
      key = input$subscription_key
    )
    data[row, ]$License <- dMeasure::simple_encode(
      paste0(
        data[row, ]$Name,
        as.character(data[row, ]$Date)
      ),
      key = input$subscription_key
    )

    record_data <- list(
      Key = data[row, ]$Key,
      License = data[row, ]$License,
      Comment = data[row, ]$Comment
    )

    subscriptions()$Subscriptions$update(
      record_id = data[row, ]$id,
      record_data = record_data
    )

    return(data)
  }

  table_DT_gui <- callModule(
    dteditmod, "subscriptions",
    thedata = table_edit,
    view.cols = c("Name", "Date", "Comment"),
    edit.cols = c("Name", "Date", "Comment"),
    callback.insert = table.insert.callback,
    callback.delete = table.delete.callback,
    callback.update = table.update.callback
  )
}

shinyApp(ui = ui, server = server)
