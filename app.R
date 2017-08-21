library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(RSQLite)
library(DT)
library(ggplot2)
library(scales)

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

# Function for inserting, updating, deleting data in sqlite database
saveData <- function(data, type="insert") {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  
  # Construct the update query by looping over the data fields
  # condition "insert" saving into database
  # condition 2 delete row from database
  # condition 3 update Beglichen
  if(type=="insert"){
    query <- sprintf(
      "INSERT INTO %s (Datum, Bewohner, EinAus, Kategorie, billdate, Wert, Kommentar, Beglichen) VALUES ('%s')",
      sqlitetable,
      paste(data, collapse = "', '")
    )
  } else if(type=="delete") { # condition for deleting a row from database, 
    query <- sprintf(
      "DELETE FROM in_out WHERE lfdNr = %i", data
    )
  } else if(type=="update") { # condition for turning Beglichen variable to TRUE
    query <- sprintf(
      "UPDATE in_out SET Beglichen = 'TRUE' WHERE lfdNr = %i", data
    )
  }
  
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
print(responses)

#READ
ReadData <- function() {
  if (exists("responses")) {
    responses
  }
}

# Fill input fields with the values of the selected record in the table
# option "Begleichen" under datatable view
# these are seperate fields for editing the data
UpdateInputs <- function(data, session) {
  updateTextInput(session, inputId = "id_tab", value = unname(data["lfdNr"]))
  updateTextInput(session, inputId = "name_tab", value = unname(data["Bewohner"]))
  updateTextInput(session, inputId = "category_tab", value = unname(data["Kategorie"]))
  updateTextInput(session, inputId = "value_tab", value = unname(data["Wert"]))
  updateCheckboxInput(session, inputId = "done_tab", value = as.logical(data["Beglichen"]))
}

EmptyInputs <- function(session) {
  updateSelectInput(session, inputId = "name", selected = "Bitte waehlen")
  updateSelectInput(session, inputId = "in_out", selected = "Bitte waehlen")
  updateSelectInput(session, inputId = "category", selected = "Bitte waehlen")
  updateDateInput(session, inputId = "billdate", value = NA)
  updateNumericInput(session, inputId = "value", value = 0)
  updateTextInput(session, inputId = "comment", value = "")
}

##########################################################################################################
############################ USER INTERFACE ##############################################################

ui = shinyUI(dashboardPage(
  skin = "green",
  
  dashboardHeader(title = "Spinnerei Kollnau"),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Eingabe", tabName = "eingabe", icon = icon("edit")),
    menuItem("Tabelle", tabName = "tabelle", icon = icon("table")),
    menuItem("Ausgabe", tabName = "ausgabe", icon = icon("line-chart"), badgeLabel = "new")
  )),
  
  dashboardBody(
    shinyjs::useShinyjs(), #use shiny js to disable the ID field
    tabItems(
      ############## INPUT TAB ##########################
      tabItem(
        tabName = "eingabe",
        fluidRow(
          selectizeInput("name", label = "Name", choices = c(
                                                          "Bitte waehlen",
                                                          "Christ1an Heym3r",
                                                          "Jan0sch Hättner",
                                                          "Chris Made1ra N0ronha",
                                                          "Johannes R3iter",
                                                          "Sophie Mai1änder",
                                                          "Miriam Zeltn3r",
                                                          "Jakob H3hl",
                                                          "Elena H3rman",
                                                          "Sz0fi",
                                                          "Felix L1ndi(ke"
                                                          )
          ),
          
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
            dateInput("billdate", "Datum der Miete / Rechnung", value = 0, format = "yyyy-mm-dd", language = "de")
          ),
          
          conditionalPanel(
            condition = "input.name != 'Bitte waehlen' && input.in_out != 'Bitte waehlen' && input.in_out != 'Bitte waehlen' && input.category != 'Bitte waehlen' && input.billdate != 0",
            numericInput("value", "Geldbetrag", value = 0)
          ),
          
          conditionalPanel(
            condition = "input.name != 'Bitte waehlen' && input.in_out != 'Bitte waehlen' && input.in_out != 'Bitte waehlen' && input.category != 'Bitte waehlen' && input.billdate != 0 && input.value != 0",
            textInput("comment", "Kommentar", NULL),
            # here you have the possibility to insert a checkboxinput for setting value "Beglichen" immediately
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
      
      
      
      ############# TABLE TAB #############################
      tabItem(
        tabName = "tabelle",

        # Create a new row for the tableview
        fluidRow(DT::dataTableOutput("responses")),
        
        # create a collapsed box above datatable view for editing the table
        box(title = "Änderungen in der Tabelle vornehmen", status = "primary", solidHeader = T, collapsible = T, collapsed = T,
          fluidRow(#actionButton("delete", "Zeile löschen"), # Implement the delete feature later maybe in an extra box
                  column(2,
                         shinyjs::disabled(textInput(inputId = "id_tab", "lfdNr", ""))
                  ),
                  column(4,
                        shinyjs::disabled(textInput(inputId = "name_tab", "Name", ""))
                  ),
                  column(4,
                         shinyjs::disabled(textInput(inputId = "category_tab", "Kategory", ""))
                  ),
                  column(2,
                         shinyjs::disabled(textInput(inputId = "value_tab", "Wert", ""))
                  ),
                    checkboxInput(inputId = "done_tab", "Beglichen"),
                    actionButton("update", "Änderung übernehmen", icon = icon("send")), # Was for submitting the change in checkbox input... maybe possible submitting while clicking the checkbox
                    actionButton("delete", "Zeile löschen (!!!)", icon = icon("trash"))
                  )
          )
        
        ),
      ################# CHARTS TAB ###############
      tabItem(tabName = "ausgabe",
                fluidRow(
                  h2("Essensausgabe pro Monat"),
                  plotOutput("plot")
                )
              )
    ))
  ))

##########################################################################################################
############################ S E R V E R #################################################################

server <- function(input, output, session) {
  
  # Update selectInput "category" after choosing selectInput "in_out"
  observeEvent(input$in_out, {
    if (input$in_out == "Ausgabe"){
      updateSelectInput(session, "category", choices = list(c("Bitte waehlen"), "Allgemein" = c("Essen", "Verbrauchsmaterialien", "Fahrtkosten"), "Verwaltung" = c("Miete", "Rueckbaukonto", "Kontoführungsgebühr", "Party", "Baumaterialien", "Überweisung Foodcoop", "Brennholz", "GEZ", "Strom", "Wasser", "Internet & Telefon", "Gas")))
    } else {updateSelectInput(session, "category", choices = c("Bitte waehlen", "Party", "Spende", "Miete"))}
  })
  
  # Update selectInput "category" after choosing selectInput "in_out"
  observeEvent(input$category, {
    if (input$category == "Miete") {updateDateInput(session, "billdate", label = "Miete für Monat?")} else {
      updateDateInput(session, "billdate", label = "Rechnungsdatum")
    }
  })
  
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
    EmptyInputs(session)
  })
  
  #Press "delete row" button
  observeEvent(input$delete, {
    # Get the right ID selected row because selected row must not equal the ID in database
    #data <- ReadData()[input$responses_rows_selected, ]
    lfdNr = loadData()[input$responses_rows_selected, ][1,1]
    saveData(data = lfdNr, type = "delete")
    # Updates the editing fields below datatable
    #UpdateInputs(CreateDefaultRecord(), session)
    responses <- loadData()
  }, priority = 1)

  #Select row in table -> show details in "Beglichen"-Inputs
  observeEvent(input$responses_rows_selected, {
    if (length(input$responses_rows_selected) > 0) {
      # maybe here it is not necessary to read in the data from CSV maybe driectly from responses RAM...
      data <- loadData()[input$responses_rows_selected, ]
      # Updates the editing fields below datatable
      UpdateInputs(data, session)
    }
  })
  
  # Update the database based on the "Begleichen" dialog below the datatable
  observeEvent(input$update, {
    # Get the "lfdNr" of data
    lfdNr = loadData()[input$responses_rows_selected, ][1,1]
    saveData(data = lfdNr, type="update")
    # update responses variable
    responses <- loadData()
  })
  
  # display table
  output$responses <- DT::renderDataTable({
    #update datatable view after submit button is clicked
    input$submit
    #update datatable view after update button is clicked
    input$update
    #update datatable view after delete button is clicked
    input$delete
    #loadData()
    datatable(loadData(), options = list(pageLength = 50, order = list(1, 'desc')), rownames = FALSE, selection = "single") %>%
      # format whole row
      #formatStyle("EinAus", target = "row", backgroundColor = styleEqual(c("Ausgabe", "Einnahme"), c('red', 'green')))
      formatStyle("EinAus", backgroundColor = styleEqual(c("Ausgabe", "Einnahme"), c('red', 'green')))
  }#, server = FALSE, selection = "single", options = list(order = list(1, 'desc'), pageLength = 50), rownames = FALSE
  )     
  
  # Create Plot
  output$plot <- renderPlot({
    # input that causes plot to renew (can be a button or else)
    input$submit
    input$delete
    input$update
    # Create plot for "Essen" only
    responses <- loadData()
    responses$newdate <- cut(as.Date(responses[["billdate"]], "%Y.%m.%d"), breaks = "month")
    ggplot(responses[responses$Kategorie == "Essen", ], aes(newdate, Wert, group = Kategorie, colour = Kategorie)) + stat_summary(fun.y = sum, geom = "bar") + xlab("Monat")# + scale_x_date(labels = date_format("%Y-%m"),breaks = "1 month")
  })
  
}


# Shiny app
shinyApp(ui = ui, server = server)