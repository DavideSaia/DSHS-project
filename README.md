# Fallimento Terapeutico della Malaria in Africa (2015–2022)

Questo progetto analizza i tassi di fallimento terapeutico nei trattamenti antimalarici in diversi paesi africani, attraverso un approccio statistico rigoroso e una visualizzazione interattiva dei dati.

##  Obiettivo
- Identificare variazioni significative nel tempo e tra paesi
- Visualizzare geograficamente i dati tramite mappa interattiva
- Applicare test di normalità e test non parametrici (Shapiro-Wilk, Kruskal-Wallis)
- Calcolare intervalli di confidenza con **bootstrap**
- Eseguire analisi post-hoc con correzioni multiple (Bonferroni, Benjamini-Hochberg)

##  Strumenti Utilizzati
- Linguaggio: `R`
- Librerie principali: `ggplot2`, `dplyr`, `shiny`, `leaflet`, `FSA`, `boot`
- Visualizzazione: `Shiny App` interattiva + grafici PDF

##  Risultati principali
- In molti paesi non si osservano differenze statisticamente significative nel tempo
- Tuttavia, alcuni casi (es. Congo, Guinea) mostrano variazioni anomale
- Il metodo bootstrap ha permesso di stimare intervalli di confidenza affidabili nonostante la non normalità dei dati

##  File principali
- `progetto_DSHS.pdf` — Report in formato pdf
- `img.zip` — Cartella zip con tutte le immagini generate
- `dataset_progetto.txt` — File txt del dataset utilizzato
- `progetto.R` — Applicazione interattiva con Leaflet e Shiny

## Note aggiuntive
Il metodo bootstrap è stato descritto e motivato in una sezione finale del report. La Shiny App permette l’esplorazione dinamica dei dati filtrando per paese e anno.

## Autori
Davide Saia, Daniele Nanni Cirulli, Giulia Giglioni, Samuele Sassi  
Corso: Data Science for Health Systems – Univ. degli Studi di Perugia  
Anno Accademico: 2024–2025

