
fluidPage(
  tags$head(
    tags$meta(charset="UTF-8"),
    tags$meta(name="description", content="Online-Tool zur Generierung von Karten anhand von Postleitzahlen und dem Amtlichen Gemeindeschlüssel. "),
    tags$meta(name="keywords", content="Generator, Karte, Postleitzahl, amtlicher Gemeindeschlüssel, shapefiles, tool, png, svg, map, Deutschland, Bundesländer, Gemeinde"),
  ), 
  
  titlePanel("Eine Karte bitte, danke!"),
  
  fluidRow(
    column(1),
    column(3,
           selectInput("map_examples", "Beispielkarten:",
                       choices=c("Einwohner pro Bundesland", 
                                 "Bevölkerungsdichte pro Bundesland (Einwohner pro km^2)", 
                                 "Stimmenanteil Tierschutzpartei Brandenburg 2017 in %"))
    )),
  
  fluidRow(
    hr(),
    column(1),
    column(3,
           h5("Online Karten Generator für Daten mit Postleitzahlen oder mit dem amtlichen Gemeindeschlüssel."),
           hr(),
           # Input: Select a file ----
           fileInput("file1", "CSV Datei hochladen", placeholder = "CSV Datei auswählen", buttonLabel = "Durchsuchen",
                     multiple = FALSE,
                     accept = c("text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
           h5("CSV Datei mit 2 Spalten. Erste Spalte kann entweder die Postleitzahl als String enthalten mit dem Spaltennahmen 'plz', oder den amtlichen Gemeindeschlüssel mit 1 bis 8 Stellen unter dem Spaltennahmen 'ags'.
             Die 2. Spalte hat den Namen 'wert' und kann beliebige numerische Werte beinhalten, die dann auf der Karte visualisiert werden. Die Geodaten stammen von http://www.bkg.bund.de © GeoBasis-DE / BKG 2020"),
           hr(),
           actionButton("reset", "Zurücksetzen"),
           actionButton("addrow", "Zeile hinzufügen"),
           actionButton("deleterow", "Zeile löschen"),
           hr(),
           h5("Sie können die Daten in der Tabelle auch direkt bearbeiten."),
           DT::dataTableOutput("daten")
           ),
    column(5,
           plotOutput(outputId = "karte", height = "700px"),
           ),
    column(3,
           textInput(inputId = "title", label = "Titel der Karte:", value = "PLZ / AGS Karte"),
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
           downloadButton('downloadSVG', 'Download SVG'),
           h5("Grafiken im SVG Format können beim Import in z.B. PowerPoint zerlegt werden und die Teile der Karte somit einzeln bearbeitet werden.")
           
           )
  ),
  fluidRow(
    hr(),
    a("Datenschutz", href="/datenschutz.html"),
    a("Impressum", href="/impressum.html")
    
  )
)