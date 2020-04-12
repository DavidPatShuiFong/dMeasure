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
    tabPanel("Subscriptions",
             h3("Subscriptions"),
             dteditUI("subscriptions")
    ),
    tabPanel("Keys",
             h3("Keys"),
             passwordInput("api_key", "API Key", value = ""),
             passwordInput("subscription_key", "table key", value = ""))
  )
)


server <- function(input, output) {

  subscriptions <- reactiveVal() # later will point to database

  table_base <- reactiveVal(data.frame(id = character(),
                                       Key = character(), License = character(),
                                       Comment = character(),
                                       stringsAsFactors = FALSE))

  table_edit <- reactiveVal(data.frame(id = character(),
                                       Key = character(), License = character(),
                                       Comment = character(),
                                       Name = character(),
                                       Date = as.Date(numeric(0), origin = "1970-01-01"),
                                       stringsAsFactors = FALSE))

  observeEvent(input$api_key, ignoreNULL = TRUE, ignoreInit = TRUE, {
    Sys.setenv("AIRTABLE_API_KEY" = input$api_key)
    subscriptions(airtabler::airtable("appLa2AH6S1SUCxE3", "Subscriptions"))
    # note that this fails very ungracefully if not a valid API_KEY
    table_base(subscriptions()$Subscriptions$select() %>% dplyr::select(c("id", "Key", "License", "Comment")))
  })

  observeEvent(input$subscription_key, ignoreNULL = TRUE, ignoreInit = TRUE, {
    dummy <- table_base()
    dummy$Name <- table_base() %>% pull(Key) %>%
      dMeasure::simple_decode(key = input$subscription_key)
    dummy$Date <- table_base() %>% pull(License) %>%
      dMeasure::simple_decode(key = input$subscription_key) %>%
      stringi::stri_sub(-10, -1) %>% # doesn't formally check the validity of the License Key
      as.Date()
    table_edit(dummy)
  })

  table.insert.callback <- function(data, row) {
    data[row,]$Name <- toupper(trimws(data[row,]$Name))
    data[row,]$Key <- dMeasure::simple_encode(data[row,]$Name,
                                              key = input$subscription_key)
    data[row,]$License <- dMeasure::simple_encode(paste0(data[row,]$Name,
                                                         as.character(data[row,]$Date)),
                                                  key = input$subscription_key)
    record_data <- list(
      Key = data[row,]$Key,
      License = data[row,]$License,
      Comment = data[row,]$Comment
    )
    new_row <- subscriptions()$Subscriptions$insert(record_data)
    data[row,]$id <- new_row$id
    return(data)
  }

  table.delete.callback <- function(data, row) {
    subscriptions()$Subscriptions$delete(data[row,]$id)
    return(data[-c(row),])
  }

  table.update.callback <- function(data, olddata, row) {
    data[row,]$Name <- toupper(trimws(data[row,]$Name))
    data[row,]$Key <- dMeasure::simple_encode(data[row,]$Name,
                                              key = input$subscription_key)
    data[row,]$License <- dMeasure::simple_encode(paste0(data[row,]$Name,
                                                         as.character(data[row,]$Date)),
                                                  key = input$subscription_key)

    record_data <- list(
      Key = data[row,]$Key,
      License = data[row,]$License,
      Comment = data[row,]$Comment
    )

    subscriptions()$Subscriptions$update(record_id = data[row,]$id,
                                         record_data = record_data)

    return(data)
  }

  table_DT_gui <- callModule(dtedit, "subscriptions", thedataframe = table_edit,
                             view.cols = c("Name", "Date", "Comment"),
                             edit.cols = c("Name", "Date", "Comment"),
                             callback.insert = table.insert.callback,
                             callback.delete = table.delete.callback,
                             callback.update = table.update.callback)
}

shinyApp(ui = ui, server = server)
