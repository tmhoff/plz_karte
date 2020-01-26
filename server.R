#https://www.suche-postleitzahl.org/downloads 
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(shiny)
library(DT)
library(datenguideR)


options(scipen=10000)

tabledata2     <- data.frame(plz = as.character(0:9), value = c(6530447, 6807966, 8529559, 8765684, 10091905, 9048173, 7379577, 8516449, 7660168, 6532345),
                          stringsAsFactors = FALSE) # Hat stringsAsFactors = TRUE als Default irgendwann jemals Sinn ergeben?
tabledata      <- data.frame(ags = as.character(1:16), value = runif(16),
                            stringsAsFactors = FALSE) # Hat stringsAsFactors = TRUE als Default irgendwann jemals Sinn ergeben?
tabledata$ags <- stringr::str_pad(tabledata$ags, width = 2, side = "left", pad = "0")
plz12345_p1      <- readRDS("data/plz_data_gesamt_p1.RDS") # als 2 Parts sind es unter 100mb mal sehen ob github es akzeptiert
plz12345_p2      <- readRDS("data/plz_data_gesamt_p2.RDS")
plz12345         <- rbind(plz12345_p1, plz12345_p2)

ags1          <- readRDS("data/ags_bundeslaender.RDS")
sample_plzs   <- c(plz12345$plz, rep(plz12345$plz, 25))

mapdata2       <- merge(plz12345, tabledata2, by = "plz")
mapdata        <- merge(ags1, tabledata, by = "ags")

server <- function(input, output) {
  
  table_data <- reactiveVal(tabledata)
  # poly_data  <- reactiveVal(ags1)
  map_data   <- reactiveVal(mapdata)
  
  api_data <- reactive({
    print("api_data")

    print(input$api_jahr)

    if (input$api_stat != "keine"){
      apidata <- as.data.frame(dg_call(nuts_nr = 1, stat_name = input$api_stat))
      print("Hallo")
      print(apidata[, c("id", "year", "value")])
      return(apidata[, c("id", "year", "value")])
    } else {
      apidata <- data.frame(id = character(), year = numeric(), value = numeric())
      return(apidata)
    }

  })
  
  possible_years <- reactive({
    print("possible_years")

    apidata <- api_data()
    unique(apidata$year)
 
  })
  
  output$ui <- renderUI({
    print("output$ui")
    selectInput("api_jahr", "Jahr auswählen:",
                choices=possible_years())
  })
  
  selected_api_data <- reactive({
    
    apidata <- api_data()
    print("HIER")
    print(input$api_jahr)
    
    print(apidata[as.character(apidata$year) == input$api_jahr, c("id", "value")])
    apidata <- apidata[as.character(apidata$year) == input$api_jahr, c("id", "value")]
    names(apidata)[1] <- "ags"
    apidata
    
  })
    
  
  
  poly_data <- reactive({
    print("poly_data")
    
    data <- table_data()
    id   <- names(data)[1]

    if (id == "plz"){
      return(plz12345)
    }
    if (id == "ags"){
      return(ags1)
    }
  })

  observeEvent(input$file1, {
    print("input$file1")
    
    data     <- data.frame(read.csv2(input$file1$datapath), stringsAsFactors = FALSE)
    data     <- data[!is.na(data[, 1]), ]
    id       <- names(data)[1]   
    
    if (id == "plz"){
      data$plz <- as.character(data$plz)
    }
    if (id == "ags"){
      data$ags <- as.character(data$ags)
    }
    
    names(data)[2] <- "value"
    data$value   <- as.numeric(data$value)
    # Update table data
    table_data(data)
    
    # Update map data
    mapdata  <- merge(poly_data(), data, by = id)
    map_data(mapdata)
    
  })
  
  observeEvent(input$addrow, {
    print("input$addrow")
    data <- table_data()
    id   <- names(data)[1]
    
    if (id == "plz"){
      if (nrow(data) > 0 ){
        data <- rbind(data.frame(plz = sample(sample_plzs, 1), value = sample(min(data$value):max(data$value), 1), stringsAsFactors = FALSE),  data)
      } else {
        data <- data.frame(plz = sample(sample_plzs, 1), value = sample(1:99999, 1), stringsAsFactors = FALSE) # :( stringsAsFactors, aaaahhh
      }
    }
    if (id == "ags"){
      ags_new <- stringr::str_pad(as.character(sample(1:16, 1)), 2, "left", "0")
      
      if (nrow(data) > 0 ){
        
        data <- rbind(data.frame(ags = ags_new, value = sample(min(data$value):max(data$value), 1), stringsAsFactors = FALSE),  data)
      } else {
        
        data <- data.frame(ags = ags_new, value = sample(1:99999, 1), stringsAsFactors = FALSE) # :( stringsAsFactors, aaaahhh
      }
    }
    
    # Update table data
    table_data(data)
    
    # Update map data
    mapdata  <- merge(poly_data(), data, by = id)
    map_data(mapdata)
    
  })
  
  observeEvent(input$api_execute, {
    print("input$api_execute")
    
    data <- selected_api_data()
    id   <- names(data)[1]
    
    # Update table data
    table_data(data)
    
    # Update map data
    mapdata  <- merge(poly_data(), data, by = id)
    map_data(mapdata)
    
  })
  
  observeEvent(input$deleterow, {
    print("input$deleterow")

    data <- table_data()
    id <- names(data)[1]   
    
    if (is.null(input$daten_rows_selected)) {
      
      data <- data[-1, ]
      
    }
    
    if (!is.null(input$daten_rows_selected)) {
      
      data <- data[-input$daten_rows_selected, ]
      
    }
    
    # Update table data
    table_data(data)
    
    # Update map data
    mapdata  <- merge(poly_data(), data, by = id)
    map_data(mapdata)
    })
  
  observeEvent(input$reset, {
    print("input$reset")
    
    # Update table data
    table_data(tabledata)
    
    # Update map data
    map_data(mapdata)
    })
  
  
  map_theme <- reactive({
    print("map_theme")
    
    #Update Theme, https://ggplot2.tidyverse.org/reference/ggtheme.html
    
    if(input$theme_choice=="default"){
      theme_bw()
    }
    else if(input$theme_choice=="minimal"){
      theme_minimal()
    }
    else if(input$theme_choice=="grau"){
      theme_gray()
    }
    else if(input$theme_choice=="dunkel"){
      theme_dark()
    }
  })
  
  color <- reactive({
    print("color")
    
    #Farbe
    if(input$color_choice=="blau"){
      "cividis"
    }
    else if(input$color_choice=="rot"){
      "inferno"
    }
    else if(input$color_choice=="grün"){
      "viridis"
    }
    else if(input$color_choice=="lila"){
      "plasma"
    }
    
  })
  
  output$karte <- renderPlot({
    print("karte")
    
    #Karte generieren
    m <- map_data() %>%
      ggplot() + 
      geom_sf() + 
      geom_sf(aes(fill = value)) + 
      scale_fill_viridis("value", option = color(), direction = -1) + 
      map_theme() + 
      guides(fill = guide_legend(title = input$legtitle)) + 
      ggtitle(input$title) + 
      theme(axis.text=element_blank(), panel.grid.major = element_blank())
    
    if (input$axislabel) m <- m + theme(axis.text=element_text())
    if (input$axislines) m <- m + theme(axis.line=element_line())
    if (input$grid)      m <- m + theme(panel.grid.major=element_line())

    m

  })
  
  output$daten <- DT::renderDataTable({
    print("daten")
    
    datatable(table_data(), selection = 'single', editable = TRUE, rownames = FALSE,
              options = list())
  })
  
  observeEvent(input$daten_cell_edit, {
    print("input$daten_cell_edit")

    data <- table_data()
    id <- names(data)[1]   
    
    info  <-  input$daten_cell_edit
    i  <- info$row 
    j  <- info$col + 1# DT nimmt sonst die falsche Spalte, ich glaube es zählt die Zeilennamen mit, vielleicht führt das zu einem Fehler beim Upload...
    if (j == 2)     v  <- as.numeric(info$value)
    if (j == 1)     v  <- as.character(info$value)
    print(j)
    print(v)
    print(data[i, j])
    data[i, j] <- v
    
    # Update
    table_data(data)
    
    mapdata  <- merge(poly_data(), data, by = id)
    map_data(mapdata)
    
  })
  
  
  
}