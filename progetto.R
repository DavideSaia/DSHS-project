# Dataset: Treatment Failure Rates by Country and Year
# Variables:
#   COUNTRY_NAME: Name of the country
#   YEAR_END: Year of observation
#   TREATMENT_FAILURE_PP: Treatment failure rate (%)
#   SAMPLE_SIZE: Sample size


malaria <- read.table(file.choose(),header=T)
head(malaria)
attach(malaria)

table(COUNTRY_NAME)#tabella di frequenza rispetto alla nazione
table(YEAR_END)#tabella di frequenza rispetto all'anno


#----------------------------------------
# # Carica i pacchetti necessari
# library(readr)
# library(dplyr)
# library(leaflet)
# 
# 
# # Supponiamo che le colonne di latitudine e longitudine si chiamino "LAT" e "LON"
# # Filtra i dati con valori non NA in TREATMENT_FAILURE_PP
# dataset_filtered <- malaria %>%
#   filter(!is.na(TREATMENT_FAILURE_PP),
#          !is.na(LATITUDE), !is.na(LONGITUDE))
# 
# # Palette colori
# pal <- ~colorNumeric("YlOrRd", dataset_filtered$TREATMENT_FAILURE_PP)(TREATMENT_FAILURE_PP)
# 
# # Crea la mappa con leaflet
# leaflet(dataset_filtered) %>%
#   addTiles() %>%
#   addCircleMarkers(
#     ~LONGITUDE, ~LATITUDE,
#     radius = 5,
#     color = ~colorNumeric("YlOrRd", dataset_filtered$TREATMENT_FAILURE_PP)(TREATMENT_FAILURE_PP),
#     popup = ~paste("TREATMENT_FAILURE_PP:", TREATMENT_FAILURE_PP)
#   ) 

#----------------------------
library(shiny)
library(leaflet)
library(dplyr)
library(readr)


# UI
ui <- fluidPage(
  titlePanel("Mappa interattiva - Treatment Failure"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Seleziona una Nazione:",
                  choices = unique(malaria$COUNTRY_NAME),
                  selected = unique(malaria$COUNTRY_NAME)[1]),
      
      selectInput("year", "Seleziona un Anno:",
                  choices = sort(unique(malaria$YEAR_END)),
                  selected = sort(unique(malaria$YEAR_END))[1])
    ),
    
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Dati filtrati reattivi
  filtered_data <- reactive({
    malaria %>%
      filter(
        COUNTRY_NAME == input$country,
        YEAR_END == input$year,
        !is.na(TREATMENT_FAILURE_PP),
        !is.na(LATITUDE),
        !is.na(LONGITUDE)
      )
  })
  
  output$map <- renderLeaflet({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      leaflet() %>%
        addTiles() %>%
        addPopups(0, 0, "Nessun dato disponibile per la selezione.")
    } else {
      pal <- colorNumeric("YlOrRd", domain = data$TREATMENT_FAILURE_PP)
      
      leaflet(data) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~LONGITUDE, lat = ~LATITUDE,
          color = ~pal(TREATMENT_FAILURE_PP),
          radius = 5,
          stroke = FALSE,
          fillOpacity = 0.8,
          popup = ~paste("Treatment Failure (%):", TREATMENT_FAILURE_PP)
        ) %>%
        addLegend(
          "bottomright",
          pal = pal,
          values = ~TREATMENT_FAILURE_PP,
          title = "Treatment Failure (%)",
          labFormat = labelFormat(suffix = "%"),
          opacity = 1
        )
    }
  })
}

# Avvia l'app
shinyApp(ui, server)


  