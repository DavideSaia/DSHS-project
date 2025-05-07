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

#----------------------------------------
# Calcolare le statistiche di base per TREATMENT_FAILURE_PP per paese
library(ggplot2)
summary_stats <- malaria %>%
  group_by(COUNTRY_NAME) %>%
  summarise(
    Media = mean(TREATMENT_FAILURE_PP, na.rm = TRUE),
    Mediana = median(TREATMENT_FAILURE_PP, na.rm = TRUE),
    Min = min(TREATMENT_FAILURE_PP, na.rm = TRUE),
    Max = max(TREATMENT_FAILURE_PP, na.rm = TRUE),
    Conteggio = n()
  )

print(summary_stats)

# Creare un grafico a boxplot per visualizzare la distribuzione di TREATMENT_FAILURE_PP per paese
p1 <- ggplot(malaria, aes(x = COUNTRY_NAME, y = TREATMENT_FAILURE_PP)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Distribuzione di TREATMENT_FAILURE_PP per Paese",
    x = "Paese",
    y = "Tasso di fallimento del trattamento (%)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1)

# Creare un istogramma per ciascun paese
p2 <- ggplot(malaria, aes(x = TREATMENT_FAILURE_PP, fill = COUNTRY_NAME)) +
  geom_histogram(bins = 10, position = "dodge", alpha = 0.7) +
  facet_wrap(~COUNTRY_NAME, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Istogrammi di TREATMENT_FAILURE_PP per Paese",
    x = "Tasso di fallimento del trattamento (%)",
    y = "Frequenza"
  ) +
  theme(legend.position = "none")

print(p2)

# Densità di TREATMENT_FAILURE_PP per paese
p3 <- ggplot(malaria, aes(x = TREATMENT_FAILURE_PP, fill = COUNTRY_NAME)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Densità di TREATMENT_FAILURE_PP per Paese",
    x = "Tasso di fallimento del trattamento (%)",
    y = "Densità"
  ) +
  theme(legend.position = "bottom", legend.title = element_blank())

print(p3)

# Visualizzazione delle frequenze assolute per paese
count_data <- malaria %>%
  group_by(COUNTRY_NAME, TREATMENT_FAILURE_PP) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(COUNTRY_NAME, TREATMENT_FAILURE_PP)

print(count_data)

# Grafico a barre per la frequenza di ogni valore di TREATMENT_FAILURE_PP per paese
p4 <- ggplot(count_data, aes(x = TREATMENT_FAILURE_PP, y = count, fill = COUNTRY_NAME)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~COUNTRY_NAME, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Frequenza dei valori di TREATMENT_FAILURE_PP per Paese",
    x = "Tasso di fallimento del trattamento (%)",
    y = "Frequenza"
  ) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

print(p4)

# Creare un violino plot per confrontare la distribuzione tra i paesi
p5 <- ggplot(malaria, aes(x = COUNTRY_NAME, y = TREATMENT_FAILURE_PP, fill = COUNTRY_NAME)) +
  geom_violin(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
  theme_minimal() +
  labs(
    title = "Distribuzione di TREATMENT_FAILURE_PP per Paese",
    x = "Paese",
    y = "Tasso di fallimento del trattamento (%)"
  ) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

print(p5)
