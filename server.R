#https://www.suche-postleitzahl.org/downloads 
#library(data.table)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(shiny)
library(DT)

server <- function(input, output) {
  
  observeEvent(input$file1, {
    
    # Derzeit muss dieser Block noch oben sthen, damit die Karte n a c h dem Upload neu erstellt wird
    data <<- data.frame(read.csv2(input$file1$datapath))
    replaceData(proxy, data, resetPaging = FALSE)  # Quelle: https://github.com/rstudio/DT/pull/480, wenn man eine ganze Zeile löscht, gibt es noch nen Error
  })
  
  data <- data.frame(plz = 0:9, N = c(6530447, 6807966, 8529559, 8765684, 10091905, 9048173, 7379577, 8516449, 7660168, 6532345))

  output$karte <- renderPlot({
    
    input$daten_cell_edit # Hiermit wird die Karte neu gerendert, wenn die Tabelle editiert wird
    input$file1 # Hiermit wird die Karte neu gerendert, wenn Daten hochgeladen werden
   
    # zeichen <- unique(nchar(data$plz))
    # if(zeichen=="1"){
    #   file_load      <- sf::st_read("data/plz-1stellig.shp")
    #   file      <- merge(file_load, data, by = "plz", all=FALSE)
    # }
    # if(zeichen=="2" & nrow(file)!=0){
    #   file_load      <- sf::st_read("data/plz-2stellig.shp")
    #   file      <- merge(file_load, file, by = "plz", all=FALSE)
    # }
    # else if(zeichen=="2" & nrow(file)==0){
    #   file_load      <- sf::st_read("data/plz-2stellig.shp")
    #   file      <- merge(file_load, data, by = "plz")
    # }
    
    if(max(nchar(data$plz))=="1"){
    file      <- sf::st_read("data/plz-1stellig.shp")
    }
    else if(max(nchar(data$plz))=="2"){
      file      <- sf::st_read("data/plz-2stellig.shp")
    }
    else if(max(nchar(data$plz))=="3"){
      file      <- sf::st_read("data/plz-3stellig.shp")
    }
    
    #merge Polygen-Daten und Daten
    file      <- merge(file, data, by = "plz")
    
    
    #Farbe
    if(input$color_choice=="blau"){
      color_karte <- "cividis"
    }
      else if(input$color_choice=="rot"){
        color_karte <- "inferno"
      }
        else if(input$color_choice=="grün"){
          color_karte <- "viridis"
        }
          else if(input$color_choice=="lila"){
            color_karte <- "plasma"
          }
    #Theme, https://ggplot2.tidyverse.org/reference/ggtheme.html
    if(input$theme_choice=="default"){
    theme_karte <- theme_bw()
    }
      else if(input$theme_choice=="minimal"){
        theme_karte <- theme_minimal()
      }
        else if(input$theme_choice=="grau"){
          theme_karte <- theme_gray()
        }
          else if(input$theme_choice=="dunkel"){
            theme_karte <- theme_dark()
          }
      
    
    #Karte generieren
    file %>%
      ggplot() + 
      geom_sf(data = file) + 
      geom_sf(aes(fill = N)) + 
      scale_fill_viridis("N", option = color_karte, direction = -1) + 
      theme_karte + 
      ggtitle("Deutschlandkarte")
    

  })
  
  
  
  output$daten  <-  renderDT(data, selection = 'none', editable = TRUE)
  
  proxy  <-  dataTableProxy('daten')
  
  observeEvent(input$daten_cell_edit, {
    info = input$daten_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    data[i, j] <<- DT::coerceValue(v, data[i, j])
    replaceData(proxy, data, resetPaging = FALSE)  # Quelle: https://github.com/rstudio/DT/pull/480, wenn man eine ganze Zeile löscht, gibt es noch nen Error
  })
  

  
}