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

# Creare un violin plot per confrontare la distribuzione tra i paesi
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

# Calcolo della deviazione standard complessiva
deviazione_totale <- sd(malaria$TREATMENT_FAILURE_PP, na.rm = TRUE)
print(paste("Deviazione standard totale:", round(deviazione_totale, 2)))

# Calcolo della deviazione standard per paese
deviazione_per_paese <- malaria %>%
  group_by(COUNTRY_NAME) %>%
  summarise(
    sd_failure = sd(TREATMENT_FAILURE_PP, na.rm = TRUE),
    .groups = "drop"
  )

print(deviazione_per_paese)


#----------------------------------------
# Check che YEAR sia trattato come numerico
malaria$YEAR_END <- as.numeric(malaria$YEAR_END)

# Calcolo della media di TREATMENT_FAILURE_PP per paese e anno
trend_data <- malaria %>%
  group_by(COUNTRY_NAME, YEAR_END) %>%
  summarise(
    avg_failure = mean(TREATMENT_FAILURE_PP, na.rm = TRUE),
    .groups = "drop"
  )

# Grafico a linee
ggplot(trend_data, aes(x = YEAR_END, y = avg_failure, color = COUNTRY_NAME)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Trend di TREATMENT_FAILURE_PP nel tempo per Paese",
       x = "Anno",
       y = "Treatment Failure (%)",
       color = "Paese") +
  theme_minimal() +
  theme(legend.position = "bottom") 


#Scatter plot: SAMPLE_SIZE vs TREATMENT_FAILURE_PP in funzione degli anni
ggplot(malaria, aes(x = SAMPLE_SIZE, y = TREATMENT_FAILURE_PP, color = as.factor(YEAR_END))) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "#000000") +
  scale_color_viridis_d(option = "plasma", name = "Anno") +
  labs(title = "Relazione tra Dimensione del Campione e Fallimento Terapeutico per Anno",
       x = "Dimensione del Campione",
       y = "Tasso di Fallimento (%)") +
  theme_minimal()

#----------------------------------------
# Scatter plot: SAMPLE_SIZE vs TREATMENT_FAILURE_PP per singolo anno
ggplot(malaria, aes(x = SAMPLE_SIZE, y = TREATMENT_FAILURE_PP)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "darkred") +
  facet_wrap(~ YEAR_END) +
  labs(title = "Relazione tra Dimensione del Campione e Fallimento Terapeutico nel Tempo",
       x = "Dimensione del Campione",
       y = "Tasso di Fallimento (%)") +
  theme_minimal()

#----------------------------------------
# 1. OUTLIER ASSOLUTO: fallimenti > 10%
outlier_assoluti <- malaria %>%
  filter(!is.na(TREATMENT_FAILURE_PP)) %>%
  filter(TREATMENT_FAILURE_PP > 10)

# Visualizzazione geografica degli outlier assoluti
ggplot(outlier_assoluti, aes(x = LONGITUDE, y = LATITUDE)) +
  geom_point(aes(size = TREATMENT_FAILURE_PP, color = COUNTRY_NAME), alpha = 0.7) +
  scale_size_continuous(name = "Failure %") +
  labs(title = "Outlier geografici con fallimento > 10%",
       x = "Longitudine", y = "Latitudine") +
  theme_minimal()

# 2. OUTLIER TEMPORALE: variazione improvvisa > 5 punti % tra anni consecutivi
outlier_temporali <- malaria %>%
  arrange(COUNTRY_NAME, YEAR_END) %>%
  group_by(COUNTRY_NAME) %>%
  mutate(diff_failure = abs(TREATMENT_FAILURE_PP - lag(TREATMENT_FAILURE_PP))) %>%
  filter(!is.na(diff_failure), diff_failure > 5)

# Visualizzazione delle variazioni anomale nel tempo
ggplot(outlier_temporali, aes(x = YEAR_END, y = TREATMENT_FAILURE_PP, group = COUNTRY_NAME)) +
  geom_line(color = "gray70") +
  geom_point(color = "red", size = 2) +
  facet_wrap(~ COUNTRY_NAME, scales = "free_y") +
  labs(title = "Variazioni anomale di fallimento > 5% tra anni consecutivi",
       x = "Anno", y = "Failure %") +
  theme_minimal()



#----------------------------------------
# Funzione per generare QQ plot per ogni paese
qq_plot_country <- function(country_data, country_name) {
  ggplot(country_data, aes(sample = TREATMENT_FAILURE_PP)) +
    stat_qq() +
    stat_qq_line() +
    ggtitle(paste("QQ Plot for", country_name)) +
    theme_minimal()
}

# QQ plot per ogni paese
unique_countries <- unique(malaria$COUNTRY)
for (country in unique_countries) {
  country_data <- subset(malaria, COUNTRY_NAME == country)
  print(qq_plot_country(country_data, country))
}


#----------------------------------------
# Esegui il test di Shapiro-Wilk sulla variabile TREATMENT_FAILURE_PP
# Test di Shapiro-Wilk per la normalità
shapiro_test_result <- shapiro.test(malaria$TREATMENT_FAILURE_PP)
# Stampa il risultato del test
print(shapiro_test_result)


# Test di Shapiro-Wilk per ogni paese
shapiro_results <- malaria %>%
  group_by(COUNTRY_NAME) %>%
  summarise(
    shapiro_test = list(shapiro.test(TREATMENT_FAILURE_PP)),
    p_value = sapply(shapiro_test, function(x) x$p.value)  # Estrai il p-value
  )
# Stampa i risultati
print(shapiro_results)


# Test di Shapiro-Wilk per ogni anno
shapiro_results_year <- malaria %>%
  group_by(YEAR_END) %>%
  summarise(
    shapiro_test = list(shapiro.test(TREATMENT_FAILURE_PP)),
    p_value = sapply(shapiro_test, function(x) x$p.value)  # Estrai il p-value
  )

# Stampa i risultati del test per anno
print(shapiro_results_year)

# Test di Shapiro-Wilk per ogni dimensione del campione
shapiro_results_size <- malaria %>%
  group_by(SAMPLE_SIZE) %>%
  filter(n() >= 3 & n() <= 5000) %>%  # Filtra i gruppi con dimensione campionaria valida
  summarise(
    shapiro_test = list(shapiro.test(TREATMENT_FAILURE_PP)),
    p_value = sapply(shapiro_test, function(x) x$p.value)  # Estrai il p-value
  )

# Stampa i risultati del test per sample size
print(shapiro_results_size)
