library(data.table)
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
    
    
    file      <- sf::st_read("data/plz-1stellig.shp")
    
    file      <- merge(file, data, by = "plz")
    file.sf   <- file['plz']
    
    file %>%
      ggplot() + 
      geom_sf(data = file.sf) + 
      geom_sf(aes(fill = N)) + 
      scale_fill_viridis("N", option = "cividis", direction = -1) + 
      theme_bw() + 
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