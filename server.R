#https://www.suche-postleitzahl.org/downloads 
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(shiny)
library(DT)


options(scipen=10000)

plz12345_p1      <- readRDS("data/plz_data_gesamt_p1.RDS") # als 2 Parts sind es unter 100mb
plz12345_p2      <- readRDS("data/plz_data_gesamt_p2.RDS")
plz12345         <- rbind(plz12345_p1, plz12345_p2)
ags1             <- readRDS("data/ags_bundeslaender.RDS")
sample_plzs      <- c(plz12345$plz, rep(plz12345$plz, 25))

server <- function(input, output) {
  
  table_data <- reactiveVal()
  map_data   <- reactiveVal()
  
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
  
  observeEvent(input$map_examples, {
    print("input$map_examples")
    
    if (input$map_examples == "Einwohner pro Bundesland"){
      data     <- data.frame(ags = as.character(1:16), value = c(2896712,
                                                                 1841179,
                                                                 7982448,
                                                                 682986,
                                                                 17932651,
                                                                 6265809, 
                                                                 4084844, 
                                                                 11069533,
                                                                 13076721, 
                                                                 990509,
                                                                 3644826, 
                                                                 2511917,
                                                                 1609675, 
                                                                 4077937, 
                                                                 2208321,
                                                                 2134393),
                                  stringsAsFactors = FALSE)
      data$ags <- stringr::str_pad(data$ags, 2, "left", "0")
      id       <- names(data)[1]   
      # Update table data
      table_data(data)
      
      # Update map data
      mapdata  <- merge(poly_data(), data, by = id)
      map_data(mapdata)
      
    }
    
    if (input$map_examples == "Bevölkerungsdichte pro Bundesland (Einwohner pro km^2)"){
      data     <- data.frame(ags = as.character(1:16), value = c(183,
                                                                 2430,
                                                                 168,
                                                                 1629,
                                                                 526,
                                                                 297, 
                                                                 206, 
                                                                 310,
                                                                 185, 
                                                                 385,
                                                                 4088, 
                                                                 85,
                                                                 69, 
                                                                 221, 
                                                                 108,
                                                                 133),
                             stringsAsFactors = FALSE)
      data$ags <- stringr::str_pad(data$ags, 2, "left", "0")
      id       <- names(data)[1]   
      # Update table data
      table_data(data)
      
      # Update map data
      mapdata  <- merge(poly_data(), data, by = id)
      map_data(mapdata)
      
    }
    
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
      ggtitle(input$title) +
      guides(fill = guide_legend(title = input$legtitle)) + 
      theme(axis.text=element_blank(), panel.grid.major = element_blank(), 
            plot.title = element_text(hjust = 0.5))
    
    if (input$axislabel)   m <- m + theme(axis.text=element_text())
    if (input$axislines)   m <- m + theme(axis.line=element_line())
    if (input$grid)        m <- m + theme(panel.grid.major=element_line())
    if (!input$showlegend) m <- m + theme(legend.position = "none")
    m <- m + theme(plot.title = element_text(size = input$titletext), 
                   legend.title = element_text(size = input$legtext))
    m

  })
  
  # downloadHandler() takes two arguments, both functions.
  # The content function is passed a filename as an argument, and
  #   it should write out data to that filename.
  output$downloadPNG <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = paste0(Sys.Date(), "_", "Karte.png"),
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      
      png(file, width = input$downloadwidth, height = input$downloadheight)
      
      #Karte generieren
      m <- map_data() %>%
        ggplot() + 
        geom_sf() + 
        geom_sf(aes(fill = value)) + 
        scale_fill_viridis("value", option = color(), direction = -1) + 
        map_theme() + 
        ggtitle(input$title) +
        guides(fill = guide_legend(title = input$legtitle)) + 
        theme(axis.text=element_blank(), panel.grid.major = element_blank(), 
              plot.title = element_text(hjust = 0.5))
      
      if (input$axislabel)   m <- m + theme(axis.text=element_text())
      if (input$axislines)   m <- m + theme(axis.line=element_line())
      if (input$grid)        m <- m + theme(panel.grid.major=element_line())
      if (!input$showlegend) m <- m + theme(legend.position = "none")
      m <- m + theme(plot.title = element_text(size = input$titletext), 
                     legend.title = element_text(size = input$legtext))
      
      print(m)
      
      dev.off()
      
    }
  )
  
  output$downloadSVG <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = paste0(Sys.Date(), "_", "Karte.svg"),
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      
      svg(file, width = input$downloadwidth, height = input$downloadheight)
      
      #Karte generieren
      m <- map_data() %>%
        ggplot() + 
        geom_sf() + 
        geom_sf(aes(fill = value)) + 
        scale_fill_viridis("value", option = color(), direction = -1) + 
        map_theme() + 
        ggtitle(input$title) +
        guides(fill = guide_legend(title = input$legtitle)) + 
        theme(axis.text=element_blank(), panel.grid.major = element_blank(), 
              plot.title = element_text(hjust = 0.5))
      
      if (input$axislabel)   m <- m + theme(axis.text=element_text())
      if (input$axislines)   m <- m + theme(axis.line=element_line())
      if (input$grid)        m <- m + theme(panel.grid.major=element_line())
      if (!input$showlegend) m <- m + theme(legend.position = "none")
      m <- m + theme(plot.title = element_text(size = input$titletext), 
                     legend.title = element_text(size = input$legtext))
      
      print(m)
      
      dev.off()
      
    }
  )
  
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