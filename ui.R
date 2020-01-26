

stats <- readRDS("data/available_stats.RDS")
stats <- stats[1:10, ]


fluidPage(
  titlePanel("Eine Karte bitte, danke!"),
  
  fluidRow(
    column(1),
    column(3,
           selectInput("map_examples", "Beispielkarten:",
                       choices=c("funktioniert noch nicht", ":( TODO"))
    ),
    column(2, h5("Daten über die API beziehen: -> TODO via datenguideR"),
           selectInput("api_stat", "Statistik auswählen:",
                       choices=c("keine", stats$stat_name))
    ),
    column(2, h5(":)"),
           uiOutput("ui")
    ),
    column(2, h5(":O"),
           actionButton("api_execute", "Ausführen")
    ),
    column(1)),
  hr(),
  fluidRow(
    column(1),
    column(3,

           # Input: Select a file ----
           fileInput("file1", "CSV Datei hochladen", placeholder = "CSV Datei auswählen", buttonLabel = "Durchsuchen",
                     multiple = FALSE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
           helpText("CSV Datei mit 2 Spalten. Erste Spalte kann entweder die Postleitzahl als String enthalten mit dem Spaltennahmen 'plz', oder den amtlichen Gemeindeschlüssel, bisher nur mit den 2 Stellen für das Bundesland unter dem Spaltennahmen 'ags'.
             Die 2. Spalte hat den Namen 'value' und kann beliebige numerische Werte beinhalten, die dann auf der Karte visualisiert werden."),
           hr(),
           actionButton("reset", "Zurücksetzen"),
           actionButton("addrow", "Zeile hinzufügen"),
           actionButton("deleterow", "Zeile löschen"),
           hr(),
           DT::dataTableOutput("daten"),
           h5("Lorem ipsum dolor sit amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquid ex ea commodi consequat. Quis aute iure reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
              Excepteur sint obcaecat cupiditat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
           ),
    column(5,
           h5("Lorem ipsum dolor sit amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquid ex ea commodi consequat. Quis aute iure reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
              Excepteur sint obcaecat cupiditat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
           plotOutput(outputId = "karte", height = "700px"),
           hr(),
           h5("Lorem ipsum dolor sit amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquid ex ea commodi consequat. Quis aute iure reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
              Excepteur sint obcaecat cupiditat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
           ),
    column(3,
           h5("Lorem ipsum dolor sit amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquid ex ea commodi consequat. Quis aute iure reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
              Excepteur sint obcaecat cupiditat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
           textInput(inputId = "title", label = "Titel der Karte:", value = "PLZ Karte"),
           textInput(inputId = "legtitle", label = "Titel der Legende:", value = "Legende"),
           selectInput("color_choice", "Farbe:",
                                       choices=c("blau", "rot", 
                                                 "grün", "lila")),
           selectInput("theme_choice", "Theme:",
                                       choices=c("minimal", "default",
                                                 "grau", "dunkel")),
           checkboxInput(inputId = "axislabel", label = "Achsenbeschriftungen"),
           checkboxInput(inputId = "axislines", label = "Achsenlinien"),
           checkboxInput(inputId = "grid", label = "Gitterlinien"),
           
           
           h5("Lorem ipsum dolor sit amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquid ex ea commodi consequat. Quis aute iure reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
              Excepteur sint obcaecat cupiditat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
    )
  )
)