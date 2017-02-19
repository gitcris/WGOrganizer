library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(RSQLite)
library(DT)

# Print current working directory
print(getwd())

# Path to sqlite database
sqlitePath <- "test.db"
# Name of the sqlite table
sqlitetable <- "in_out"

# Function for creating a new database if none already exists
createDatabase <- function() {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the update query by looping over the data fields
  createUsers <- "INSERT INTO %s (Datum, Bewohner, EinAus, Kategorie, billdate, Wert, Kommentar, Beglichen) VALUES ('%s')"
  createResponses <- ""
  createCounters <- ""
  
  # Submit the create queries and disconnect from database
  dbGetQuery(db, createUsers)
  dbGetQuery(db, createResponses)
  dbGetQuery(db, createCounters)
  dbDisconnect(db) 
}

generateRandomData <- function(number) {
  for (i in 1:number) {
    Bewohner <-
      c("Chris", "Sophie", "Jo", "Julia", "Miri")[sample(1:5, 1, replace = T)]
    Datum <- format(Sys.time(), "%Y.%m.%d %H:%M:%S")
    Wert <- sample(1:100, 1, replace = T)
    Kategorie <-
      c("Essen",
        "Fahrtkosten",
        "Kueche",
        "Party",
        "Spende",
        "Materialien")[sample(1:6, 1, replace = T)]
    entry <- c(Datum, Bewohner, "Ausgabe", Kategorie, Wert, TRUE)
    saveData(entry)
  }
}

# Function for saving the data in sqlite database
saveData <- function(data) {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (Datum, Bewohner, EinAus, Kategorie, billdate, Wert, Kommentar, Beglichen) VALUES ('%s')",
    sqlitetable,
    #paste(names(data), collapse = "', '")
    paste(data, collapse = "', '")
    #     paste(names(data), collapse = ", "),
    #     paste(data, collapse = "', '")
  )
  # Submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
}

# Function for loading the data from sqlite database
loadData <- function() {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", sqlitetable)
  # Submit the fetch query and disconnect
  responses <- dbGetQuery(db, query)
  dbDisconnect(db)
  return(responses)
}

# Load database at start for populating the selectInputs of user interface
responses <- loadData()


# Get table metadata. For now, just the fields
# Further development: also define field types
# and create inputs generically
GetTableMetadata <- function() {
  fields <- c(id = "Id",
              date = "Datum",
              name = "Name",
              in_out = "Einnahme / Ausgabe",
              category = "Kategorie",
              billdate = "Miet- / Rechnungsdatum",
              value = "Wert",
              comment = "Kommentar",
              done = "Beglichen")
  
  result <- list(fields = fields)
  return (result)
}

#CREATE
CreateData <- function(data) {
  data <- CastData(data)
  rownames(data) <- GetNextId()
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

#READ
ReadData <- function() {
  if (exists("responses")) {
    responses
  }
}

#UPDATE
UpdateData <- function(data) {
  data <- CastData(data)
  responses[row.names(responses) == row.names(data), ] <<- data
}

#DELETE
DeleteData <- function(data) {
  responses <<- responses[row.names(responses) != unname(data["id"]), ]
}

# Cast from Inputs to a one-row data.frame
CastData <- function(data) {
  datar <- data.frame(date = data["date"],
                      name = data["name"],
                      in_out = data["in_out"],
                      category = data["category"],
                      billdate = data["billdate"],
                      value = as.integer(data["value"]),
                      comment = data["comment"],
                      done = as.logical(data["done"]),
                      stringsAsFactors = FALSE)
  
  rownames(datar) <- data["id"]
  return (datar)
}

# Fill input fields with the values of the selected record in the table
# these are seperate fields for editing below the data table
UpdateInputs <- function(data, session) {
  updateTextInput(session, inputId = "id_tab", value = unname(rownames(data)))
  updateTextInput(session, inputId = "name_tab", value = unname(data["name"]))
  updateCheckboxInput(session, inputId = "done_tab", value = as.logical(data["done"]))
}

EmptyInputs <- function(session) {
  updateSelectInput(session, inputId = "name", selected = "Bitte waehlen")
  updateSelectInput(session, inputId = "in_out", selected = "Bitte waehlen")
  updateSelectInput(session, inputId = "category", selected = "Bitte waehlen")
  updateDateInput(session, inputId = "billdate", value = 0)
  updateNumericInput(session, inputId = "value", value = 0)
  updateTextInput(session, inputId = "comment", value = "")
}

##########################################################################################################
############################ USER INTERFACE ##############################################################
##########################################################################################################

ui = shinyUI(dashboardPage(
  skin = "green",
  
  dashboardHeader(title = "Spinnerei Kollnau"),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Eingabe", tabName = "eingabe", icon = icon("edit")),
    menuItem("Tabelle", tabName = "tabelle", icon = icon("table")),
    menuItem("Ausgabe", tabName = "ausgabe", icon = icon("line-chart"))
  )),
  
  dashboardBody(  #use shiny js to disable the ID field
    shinyjs::useShinyjs(),
    tabItems(
      ############## Eingabe-Tab ##########################
      tabItem(
        tabName = "eingabe",
        fluidRow(
          selectInput("name", label = "Name", choices = c(
                                                          "Bitte waehlen",
                                                          "Christian Heymer",
                                                          "Janosch Hüttner",
                                                          "Chris Madeira Noronha",
                                                          "Johannes Reiter",
                                                          "Sophie Mailänder",
                                                          "Miriam Zeltner",
                                                          "Jakob Hehl",
                                                          "Amir",
                                                          "Szofi",
                                                          "Felix Lindicke"
                                                          )
          ),
          
          # Implement later or just delete
          #checkboxInput("check", label = "Optionen aktiviert", FALSE),
          
          conditionalPanel(
            condition = "input.name != 'Bitte waehlen'",
            selectInput(
              "in_out",
              label = "Aus- oder Einnahme",
              choices = c("Bitte waehlen", "Einnahme", "Ausgabe")
            )
          ),
          
          # choose category of transaction
          conditionalPanel(
            condition = "input.name != 'Bitte waehlen' && input.in_out != 'Bitte waehlen' && input.in_out != 'Bitte waehlen'",
            selectInput(
              "category",
              label = "Kategorie",
              choices = c("Bitte waehlen",
                          "Party",
                          "Spende",
                          "Miete")
            )
          ),
          
          conditionalPanel(
            condition = "input.name != 'Bitte waehlen' && input.in_out != 'Bitte waehlen' && input.in_out != 'Bitte waehlen' && input.category != 'Bitte waehlen'",
            dateInput("billdate", "Datum der Miete / Rechnung", value = "", format = "yyyy-mm-dd", language = "de")
          ),
          
          conditionalPanel(
            condition = "input.name != 'Bitte waehlen' && input.in_out != 'Bitte waehlen' && input.in_out != 'Bitte waehlen' && input.category != 'Bitte waehlen' && input.billdate != ''",
            numericInput("value", "Geldbetrag", value = 0)
          ),
          
          conditionalPanel(
            condition = "input.name != 'Bitte waehlen' && input.in_out != 'Bitte waehlen' && input.in_out != 'Bitte waehlen' && input.category != 'Bitte waehlen' && input.billdate != 0 && input.value != 0",
            textInput("comment", "Kommentar", NULL),
            # Implement later... for making "Beglichen" immediately
            #checkboxInput("done", "Beglichen", FALSE),
            helpText("Nach der Eingabe einfach auf",
                     "den Abschicken-Knopf druecken."),
            actionButton("submit", "Abschicken", icon = icon("send"))
          ),

        # dialog forcing user to check the input values (triggered by clicking on submit)
        bsModal(
          id = "confirm",
          title = "In Datenbank übernehmen?",
          trigger = "submit",
          #size = "small",
          HTML("Bitte überprüfe nochmal deine Eingabe!<br>Möchtest du den neuen Eintrag zur Datenbank hinzufügen?
               <br>Danke.<br>
               "),
          actionButton("BUTyes", "Jaaaa!")
          )
        
        )
      ),
      
      
      
      ############# Tabellen-Tab #############################
      tabItem(
        tabName = "tabelle",
        
        # Create a new Row in the UI for selectInputs
        # fluidRow(
        #   column(3,
        #          dateRangeInput("daterange", "Datum")),
        #   column(2,
        #          selectInput("user", "Benutzer", c(
        #            "Alles", unique(as.character(responses$Bewohner))
        #          )))
        #   ),
        
        # Create a new row for the table.
        fluidRow(DT::dataTableOutput("responses")),

        box(title = "Daten bearbeiten", status = "primary", solidHeader = T, collapsible = T, collapsed = T,
          fluidRow(actionButton("delete", "Zeile löschen"),
                  shinyjs::disabled(textInput(inputId = "id_tab", "ID", "")),
                  shinyjs::disabled(textInput(inputId = "name_tab", "Name", "")),
                  checkboxInput(inputId = "done_tab", "Beglichen", FALSE),
                  actionButton("submit", "Abschicken", icon = icon("send"))
                   )
          )
        ),
      tabItem(tabName = "ausgabe",
              h2("Ausgabe"))
    ))
  ))

##########################################################################################################
############################ S E R V E R #################################################################
##########################################################################################################

server <- function(input, output, session) {
  
  # Update selectInput "category" after choosing selectInput "in_out"
  # Implement later the "Optionen" for in and outs that are not often used like Wasser, Strom ...
  # observeEvent(input$in_out, {
  #   if (input$in_out == "Ausgabe" && input$check == TRUE) {updateSelectInput(session, "category", choices = c("Bitte waehlen", "Essen", "Fahrtkosten", "Party", "Strom", "Holz", "Gas", "Wasser", "Telefon"))}
  #   else if (input$in_out == "Ausgabe" && input$check == FALSE) {
  #     updateSelectInput(session, "category", choices = c("Bitte waehlen", "Essen", "Fahrtkosten", "Party"))
  #   } else {updateSelectInput(session, "category", choices = c("Bitte waehlen", "Party", "Spende", "Miete"))}
  # })
  
  # Update selectInput "category" after choosing selectInput "in_out"
  observeEvent(input$in_out, {
    if (input$in_out == "Ausgabe"){
      updateSelectInput(session, "category", choices = c("Bitte waehlen", "Essen", "Fahrtkosten", "Party", "Holz", "GEZ", "Strom", "Wasser", "Internet & Telefon", "Gas"))
    } else {updateSelectInput(session, "category", choices = c("Bitte waehlen", "Party", "Spende", "Miete"))}
  })
  
  # Update selectInput "category" after choosing selectInput "in_out"
  observeEvent(input$category, {
    if (input$category == "Miete") {updateDateInput(session, "billdate", label = "Miete für Monat?")} else {
      updateDateInput(session, "billdate", label = "Rechnungsdatum")
    }
  })
  
  # input fields are treated as a group
  # formData <- reactive({
  #   sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
  # })
  
  # Click submit button and confirm with Yes
  observeEvent(input$BUTyes, {
    toggleModal(session, "confirm", toggle = "close")
    # Save values to database...
    newdata <-
      c(
        format(Sys.time(), "%Y.%m.%d %H:%M:%S"),
        input$name,
        input$in_out,
        input$category,
        format(input$billdate, "%Y.%m.%d"),
        as.numeric(input$value),
        input$comment,
        # Populate the done input with default FALSE
        #input$done
        FALSE
      )
    # print input values for debugging...
    #print(newdata)
    saveData(newdata)
    
    # Deactivated emptying the input fields for debugging...
    #EmptyInputs(session)
  })
  
  # Click "Submit" button -> save data
  # observeEvent(input$BUTyes, {
  #   toggleModal(session, "confirm")#, toggle = "close")
  #   if (input$id != "0") {
  #     UpdateData(formData())
  #   } else {
  #     CreateData(formData())
  #     # Updates the editing fields below datatable
  #     UpdateInputs(CreateDefaultRecord(), session)
  #   }
  # }, priority = 1)

  # Press "Delete" button -> delete from data
  # observeEvent(input$delete, {
  #   DeleteData(formData())
  #   # Updates the editing fields below datatable
  #   UpdateInputs(CreateDefaultRecord(), session)
  # }, priority = 1)

  # Select row in table -> show details in inputs
  # observeEvent(input$responses_rows_selected, {
  #   if (length(input$responses_rows_selected) > 0) {
  #     # maybe here it is not necessary to read in the data from CSV maybe driectly from responses RAM...
  #     data <- ReadData()[input$responses_rows_selected, ]
  #     # Updates the editing fields below datatable
  #     UpdateInputs(data, session)
  #   }
  # })
  
  # display table
  output$responses <- DT::renderDataTable({
    #update after submit is clicked
    input$submit
    #update after delete is clicked
    input$delete
    loadData()
  }, server = FALSE, selection = "single", options = list(order = list(2, 'desc'), pageLength = 50)
  #colnames = unname(GetTableMetadata()$fields)[-1]
  )     
  
  
}


# Shiny app
shinyApp(ui = ui, server = server)