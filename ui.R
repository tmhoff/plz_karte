fluidPage(
  titlePanel("PLZ Karte"),
  
  fluidRow(
    column(1
    ),
    
    column(4,
           # Input: Select a file ----
           fileInput("file1", "CSV Datei hochladen",
                     multiple = FALSE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
           DT::dataTableOutput("daten")
           ),
    
    column(1),
    
    column(5,
           plotOutput(outputId = "karte")
           ),
    
    column(1)
    
    
    
  )
  

  
)