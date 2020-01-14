fluidPage(
  titlePanel("PLZ Karte"),
  
  fluidRow(
    column(1),
    
    column(3,
           # Input: Select a file ----
           fileInput("file1", "CSV Datei hochladen (plz, N)",
                     multiple = FALSE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
           hr(),
           helpText("Zur Zeit nur PLZ 1,2,3-Steller möglich"),
           DT::dataTableOutput("daten")
           ),
    #Dropdown einfügen Farbe
    sidebarPanel(
      selectInput("color_choice", "Farbe:",
                  choices=c("blau", "rot","grün","lila"))#,
      # hr(),
      # helpText("Bitte Waehle die Anzahl der Ziffern fuer die PLZ")
    ),
    #Dropdown einfügen Theme
    sidebarPanel(
      selectInput("theme_choice", "Theme:",
                  choices=c("default", "minimal","grau", "dunkel"))#,
      # hr(),
      # helpText("Bitte Waehle die Anzahl der Ziffern fuer die PLZ")
    ),
    column(1),

    column(8,
           plotOutput(outputId = "karte")
           ),
    
    column(1)
    

    
  )
  

  
)