
fluidPage(
  titlePanel("Eine Karte bitte, danke!"),
  
  fluidRow(
    column(1),
    column(3,
           selectInput("map_examples", "Beispielkarten:",
                       choices=c("Einwohner pro Bundesland", 
                                 "Bevölkerungsdichte pro Bundesland (Einwohner pro km^2)"))
    ),
  hr()),
  
  fluidRow(
    column(1),
    column(3,

           # Input: Select a file ----
           fileInput("file1", "CSV Datei hochladen", placeholder = "CSV Datei auswählen", buttonLabel = "Durchsuchen",
                     multiple = FALSE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
           h5("CSV Datei mit 2 Spalten. Erste Spalte kann entweder die Postleitzahl als String enthalten mit dem Spaltennahmen 'plz', oder den amtlichen Gemeindeschlüssel mit 1 bis 8 Stellen unter dem Spaltennahmen 'ags'.
             Die 2. Spalte hat den Namen 'value' und kann beliebige numerische Werte beinhalten, die dann auf der Karte visualisiert werden. Die Geodaten stammen von http://www.bkg.bund.de © GeoBasis-DE / BKG 2020"),
           hr(),
           actionButton("reset", "Zurücksetzen"),
           actionButton("addrow", "Zeile hinzufügen"),
           actionButton("deleterow", "Zeile löschen"),
           hr(),
           DT::dataTableOutput("daten")
           ),
    column(5,
           plotOutput(outputId = "karte", height = "700px"),
           hr()
           ),
    column(3,
           textInput(inputId = "title", label = "Titel der Karte:", value = "PLZ Karte"),
           numericInput(inputId = "titletext", label = "Schriftgöße Titel:", min = 6, max = 50, value = 12),
           textInput(inputId = "legtitle", label = "Titel der Legende:", value = "Legende"),
           numericInput(inputId = "legtext", label = "Schriftgöße Legendentitel:", min = 6, max = 50, value = 12),
           checkboxInput(inputId = "showlegend", label = "Legende einblenden", value = TRUE),
           selectInput("color_choice", "Farbe:",
                                       choices=c("blau", "rot", 
                                                 "grün", "lila")),
           selectInput("theme_choice", "Theme:",
                                       choices=c("minimal", "default",
                                                 "grau", "dunkel")),
           checkboxInput(inputId = "axislabel", label = "Achsenbeschriftungen"),
           checkboxInput(inputId = "axislines", label = "Achsenlinien"),
           checkboxInput(inputId = "grid", label = "Gitterlinien"),
           numericInput(inputId = "downloadwidth", "Breite der Datei in Pixel", min = 200, max = 5000, value = 600),
           numericInput(inputId = "downloadheight", "Höhe der Datei in Pixel", min = 200, max = 5000, value = 800),
           downloadButton('downloadPNG', 'Download PNG'),
           downloadButton('downloadSVG', 'Download SVG')
           )
  )
)