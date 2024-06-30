## codice per creare una visualizzazione del mix elettrico della rete californiana
# https://www.gridstatus.io/
# https://bpmcm.caiso.com/BPM%20Document%20Library/Market%20Instruments/BPM_for_Market%20Instruments_V85_Redline.pdf

## scaricamendo dei dati con le API di gridstatus
## elaborazione per adattarli allo scopo

rm(list = ls())
setwd("...")

### librerie con dev per ggplot2
#      per https://github.com/tidyverse/ggplot2/pull/5777
#####
# lista delle librerie necessarie
librerie <- c("dplyr", "forcats", "ggplot2", "ggtext", "glue", "httr", "jsonlite", "lubridate", 
              "png", "purrr", "RColorBrewer", "readr", "readxl", "scales", "stringr", 
              "systemfonts", "showtext", "tibble", "tidyr", "webshot", "webshot2", "writexl", "zoo")

# ciclo per controllare se sono installate e successivamente importarle
for(libreria in librerie){
  if(!libreria %in% installed.packages()){
    # installazione ggplot2 separatamente usando la versione di sviluppo
    if(libreria == "ggplot2") {
      if(!requireNamespace("devtools", quietly = TRUE))
        install.packages("devtools")
      devtools::install_github("tidyverse/ggplot2", force = TRUE)
    } else {
      install.packages(libreria)
    }
  }
  library(libreria, character.only = TRUE)
}
#####


## download dati con API e salvataggio in csv
# https://docs.gridstatus.io/en/stable/api-reference.html
# https://api.gridstatus.io/docs
#####
api_key <- "..."
base_url <- "https://api.gridstatus.io/v1/datasets"

# funzione per costruire l'URL dell'API e effettuare la richiesta con filtraggio e gestione della paginazione
# essendoci dei limiti di estrazione dati bisogna ottimizzare la richiesta
make_request <- function(dataset_id, filter_column, filter_value, start_time, end_time, publish_time, format = "json") {
  all_data <- data.frame()
  # per iniziare dalla prima pagina
  page <- 1  
  # flag di controllo per il fetching dei dati
  keep_fetching <- TRUE  
  
  while (keep_fetching) {
    # preparazione dell'URL e dei parametri di query
    url <- sprintf("%s/%s/query", base_url, dataset_id)
    query_params <- list(
      api_key = api_key,
      start_time = start_time,
      end_time = end_time,
      publish_time = publish_time,
      return_format = format,
      page_size = 500,  # regolazione basata sulle prestazioni dell'API e sulla dimensione della risposta
      page = page
    )
    # aggiunta dei parametri di filtro se specificati
    if (!is.null(filter_column) && !is.null(filter_value)) {
      query_params$filter_column <- filter_column
      query_params$filter_value <- filter_value
    }
    
    # richiesta HTTP
    response <- GET(url, query = query_params)
    if (status_code(response) != 200) {
      stop("Impossibile scaricare i dati con il codice di stato ", status_code(response), ": ", content(response, "text", encoding = "UTF-8"), call. = FALSE)
    }
    
    # parsing della risposta JSON
    data <- content(response, type = "text", encoding = "UTF-8")
    json_data <- fromJSON(data)
    
    # aggiunta dei dati scaricati al dataframe all_data
    if (length(json_data$data) > 0) {
      all_data <- rbind(all_data, as.data.frame(json_data$data))
    } else {
      # interruzione fetching se non vengono restituiti dati
      keep_fetching <- FALSE  
    }
    
    # verifica dell'esistenza di una pagina successiva da scaricare
    if (!is.null(json_data$meta) && json_data$meta$hasNextPage) {
      page <- page + 1
    } else {
      keep_fetching <- FALSE
    }
  }
  
  return(all_data)
}

# funzione per convertire data-ora UTC in dati orari del Pacific Time
convert_time <- function(dataset) {
  # verifica per il dataset vuoto
  if (nrow(dataset) == 0) {
    return(data.frame())
  }
  
  # verifica perla presenza delle colonne necessarie
  required_columns <- c("interval_start_utc", "interval_end_utc")
  if (!all(required_columns %in% names(dataset))) {
    stop("Il dataset manca di una o più colonne richieste.")
  }
  # conversione delle colonne di data-ora UTC in dati orari
  dataset$interval_start_utc <- as.POSIXct(dataset$interval_start_utc, format="%Y-%m-%dT%H:%M:%S", tz="UTC")
  dataset$interval_end_utc <- as.POSIXct(dataset$interval_end_utc, format="%Y-%m-%dT%H:%M:%S", tz="UTC")
  
  # applicazione del fuso orario Pacific Time
  dataset$interval_start_utc <- with_tz(dataset$interval_start_utc, "America/Los_Angeles")
  dataset$interval_end_utc <- with_tz(dataset$interval_end_utc, "America/Los_Angeles")
  return(dataset)
}

# configurazione dell'orario (regolato a UTC)
start_time <- "2024-04-29T07:00Z"  # orario UTC per 09:00 Europe/Rome
end_time <- "2024-06-03T07:00Z"    # orario UTC per 09:00 Europe/Rome

# configurazione dei dataset da scaricare come elementi di una lista
datasets_config <- list(
  caiso_fuel_mix = list(id="caiso_fuel_mix", filter_column=NULL, filter_value=NULL),
  caiso_lmp_day_ahead_hourly = list(id="caiso_lmp_day_ahead_hourly", filter_column="location", filter_value="TH_SP15_GEN-APND"),
  caiso_lmp_real_time_15_min = list(id="caiso_lmp_real_time_15_min", filter_column="location", filter_value="TH_SP15_GEN-APND"),
  caiso_load = list(id="caiso_load", filter_column=NULL, filter_value=NULL),
  caiso_curtailment_aggregated_hourly = list(id="caiso_curtailment_aggregated_hourly", filter_column=NULL, filter_value=NULL)
)

# lista per conservare i dataset scaricati
datasets <- list()

## chiamate API, conversione data e salvataggio in csv
for (dataset_name in names(datasets_config)) {
  
  # impostazioni per le API
  config <- datasets_config[[dataset_name]]
  # chiamata alla funzione make_request per scaricare i dati con le API
  dataset <- make_request(config$id, config$filter_column, config$filter_value, start_time, end_time, "latest")
  
  # verifica per il dataset vuoto
  if (nrow(dataset) == 0) {
    cat("Nessun dato disponibile per il salvataggio di", paste0(config$id, ".csv"), "\n")
    # passa al rossimo ciclo del for() se non ci sono dati
    next
  }
  
  # conversione delle colonne di data-ora UTC in dati orari del Pacific Time
  dataset <- convert_time(dataset)
  
  # salvataggio del dataset nella lista
  datasets[[dataset_name]] <- dataset
  
  # salvataggio del dataset in CSV con controllo errore
  tryCatch({
    write.csv(dataset, file = paste0(config$id, ".csv"), row.names = FALSE)
    cat("Dati salvati con successo in", paste0(config$id, ".csv"), "\n")
  }, error = function(e) {
    cat("Si è verificato un errore durante il salvataggio del CSV:", e$message, "\n")
  })
}
#####

## aggregazione fuel mix che non è solare, batterie, import - export (e salvataggio csv)
#####
# lettura dati importati con API e salvati in csv
updated_caiso_fuel_mix <- datasets[["caiso_fuel_mix"]]

# rinomino colonne
updated_caiso_fuel_mix <- updated_caiso_fuel_mix %>%
  rename(
    solare = solar,
    batterie = batteries,
    `import - export` = imports
  )

glimpse(updated_caiso_fuel_mix)

# aggiunta e modifica colonne
updated_caiso_fuel_mix <- updated_caiso_fuel_mix %>%
  # calcolo della produzione rimanente escludendo specifiche colonne energetiche
  mutate(
    produzione_rimanente = rowSums(
      select(., wind, geothermal, biomass, biogas, small_hydro, coal, nuclear, natural_gas, large_hydro, other),
      na.rm = TRUE
    )
  ) %>%
  # per applicare pmax() bisogna fare l'operazione riga per riga
  rowwise() %>%
  # calcolo di total_gross_mix e total_net_mix esplicitando le colonne
  mutate(
    # mix totale (sommando solo i valori positivi)
    total_gross_mix = sum(solare, pmax(batterie, 0), pmax(`import - export`, 0), produzione_rimanente, 0, na.rm = TRUE),
    # mix totale netto (sommando tutti i valori, sia positivi che negativi)
    total_net_mix = sum(solare, batterie, `import - export`, produzione_rimanente, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # selezione delle colonne di interesse
  select(interval_start_utc, interval_end_utc, solare, batterie, `import - export`, produzione_rimanente, total_gross_mix, total_net_mix)

glimpse(updated_caiso_fuel_mix)

write_csv(updated_caiso_fuel_mix, "updated_caiso_fuel_mix.csv")
#####

# lettura (altri) dataset importati con API
caiso_lmp_day_ahead_hourly <- datasets[["caiso_lmp_day_ahead_hourly"]]
caiso_lmp_real_time_15_min <- datasets[["caiso_lmp_real_time_15_min"]]
caiso_load <- datasets[["caiso_load"]]
caiso_curtailment <- datasets[["caiso_curtailment_aggregated_hourly"]]

## osservazioni varie
#     scelta fra curtailment solare MW o MWh
#####
# colonne di data-ora da stringa a POSIXct
caiso_curtailment <- caiso_curtailment %>%
  # calcolo differenza e differenza cumulata
  mutate(
    difference = solar_curtailment_mw - solar_curtailment_mwh,
    cumulative_difference = cumsum(difference)
  )

# grafico a linee per confronto
ggplot(caiso_curtailment, aes(x = interval_start_utc)) +
  geom_line(aes(y = solar_curtailment_mwh, color = "solar_curtailment_mwh"), size = 1) +
  geom_line(aes(y = solar_curtailment_mw, color = "solar_curtailment_mw"), size = 1) +
  geom_line(aes(y = difference, color = "difference"), size = 1) +
  geom_line(aes(y = cumulative_difference, color = "cumulative_difference"), size = 1) +
  labs(
    title = "Curtailment Solare Aggregato Orario",
    x = "",
    y = "log(Curtailment)"
  ) +
  scale_color_manual(
    values = c("solar_curtailment_mwh" = "blue", "solar_curtailment_mw" = "red", "difference" = "#ffd700", "cumulative_difference" = "black"),
    breaks = c("solar_curtailment_mwh", "solar_curtailment_mw", "difference", "cumulative_difference"),
    labels = c("Curtailment Solare (MWh)", "Curtailment Solare (MW)", "Differenza (MW - MWh)", "Differenza Cumulativa")
  ) +
  scale_y_log10(
    breaks = c(0, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000, 150000),
    # notazione classica
    labels = scales::comma
  )  +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.y = element_line(color = lightgray, linetype = "dotted"),  # griglia principale sull'asse y
    panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_line(color = "black")  # tics sull'asse y
  )
#####

## aggregazione tutti i df facendo media settimanale (e salvataggio csv)
#####
## aggregazione

# filtraggio solo colonne utili
caiso_lmp_day_ahead_hourly_filtered <- caiso_lmp_day_ahead_hourly %>%
  filter(location == "TH_SP15_GEN-APND") %>%
  select(interval_start_utc, interval_end_utc, lmp_day_ahead_hourly = lmp)

caiso_lmp_real_time_15_min_filtered <- caiso_lmp_real_time_15_min %>%
  filter(location == "TH_SP15_GEN-APND") %>%
  select(interval_start_utc, interval_end_utc, lmp_real_time_15_min = lmp)

caiso_curtailment_solar <- caiso_curtailment %>%
  select(interval_start_utc, interval_end_utc, solar_curtailment_mw)


## estensione ed aggiustamento dati per day-ahead hourly e per real-time 15-min

# creazione sequenza completa di timestamp ogni 5 minuti
all_timestamps <- seq(from = min(updated_caiso_fuel_mix$interval_start_utc),
                      # se metto end includo anche il dato che *parte* alle 00:00
                      to = max(updated_caiso_fuel_mix$interval_start_utc),
                      by = "5 min")

# funzione per estendere il dataset aggiungendo una riga con l'ultimo valore conosciuto
# questo perché quando poi si fa smooth serve che ci sia un valore finale
extend_data_with_final_row <- function(df) {
  last_row <- tail(df, 1)
  final_row <- last_row
  final_row$interval_start_utc <- last_row$interval_end_utc
  final_row$interval_end_utc <- last_row$interval_end_utc + minutes(5)
  extended_df <- rbind(df, final_row)
  return(extended_df)
}

# funzione per applicare estensione o smoothing ai dati
extend_and_smooth <- function(df, value_col, all_timestamps, smooth = FALSE) {
  df <- df  %>% 
    # estenzione dati con una riga finale
    extend_data_with_final_row() %>%
    # aggiunta timestamp ogni 5m
    complete(interval_start_utc = all_timestamps) %>%
    # adattamento timestammp _end
    mutate(interval_end_utc = interval_start_utc + minutes(5)) %>%
    arrange(interval_start_utc)
  
  if (smooth) {
    # applicazione smoothing tramite interpolazione lineare
    df <- df %>%
      mutate(!!paste0(value_col, "_smooth") := na.approx(!!sym(value_col), na.rm = FALSE))
  } else {
    # rimepimento NA senza applicare smoothing
    #   riempiendo i valori NA con il valore non-NA precedente
    df <- df %>%
      fill(all_of(value_col), .direction = "downup")
  }
  
  return(df)
}


# applicazione funzioni alle serie di dati
# per i prezzi mantengo l'andamento a gradini e quindi non faccio smooth
caiso_lmp_day_ahead_hourly_extended <- extend_and_smooth(df = caiso_lmp_day_ahead_hourly_filtered, value_col = "lmp_day_ahead_hourly", all_timestamps = all_timestamps, smooth = FALSE)
caiso_lmp_real_time_15_min_extended <- extend_and_smooth(df = caiso_lmp_real_time_15_min_filtered, value_col = "lmp_real_time_15_min", all_timestamps = all_timestamps, smooth = FALSE)
caiso_curtailment_solar_smooth <- extend_and_smooth(df = caiso_curtailment_solar, value_col = "solar_curtailment_mw", all_timestamps = all_timestamps, smooth = TRUE)

glimpse(updated_caiso_fuel_mix)
glimpse(caiso_lmp_day_ahead_hourly_extended)
glimpse(caiso_lmp_real_time_15_min_extended)
glimpse(caiso_curtailment_solar_smooth)
glimpse(caiso_load)

# controllo le ultime righe dei dataset estesi per vedere se `smooth` è stato applicato correttamente
tail(caiso_lmp_day_ahead_hourly_extended)
tail(caiso_lmp_real_time_15_min_extended)
tail(caiso_curtailment_solar_smooth)

# unione dei dataset gestendo le colonne duplicate
# ci sono altre colonne doppie come interval_end_utc
all_data <- updated_caiso_fuel_mix %>%
  left_join(caiso_curtailment_solar_smooth %>% 
              select(-interval_end_utc),
            by = "interval_start_utc") %>%
  left_join(caiso_lmp_day_ahead_hourly_extended %>% 
              select(-interval_end_utc),
            by = "interval_start_utc") %>%
  left_join(caiso_lmp_real_time_15_min_extended %>% 
              select(-interval_end_utc),
            by = "interval_start_utc") %>%
  left_join(caiso_load %>% 
              select(-interval_end_utc),
            by = "interval_start_utc")

glimpse(all_data)

# 10080 righe con intervalli di 5m:
#  10080/(60/5)/24 = 35 -> converma che sono 5 settimane complete


## aggregazione tramite  media settimanale

# operazioni preliminari per strutturare il df
all_data <- all_data %>%
  mutate(
    # aggiunta dei giorni della settimana in italiano
    week_start = wday(interval_start_utc, label = TRUE, abbr = FALSE),
    week_end = wday(interval_end_utc, label = TRUE, abbr = FALSE),
    # formattazione delle ore
    hour_start = format(interval_start_utc, "%H:%M:%S"),
    hour_end = format(interval_end_utc, "%H:%M:%S"),
    # creazione stringhe che combinano il giorno della settimana e l'orario
    week_hour_start = paste(week_start, hour_start),
    week_hour_end = paste(week_end, hour_end)
  )

glimpse(all_data)

# calcolo della media settimanale aggiornando la chiamata a across()
weekly_avg_data <- all_data %>%
  # tolgo gli _end tanto sono inutili, basterebbe solo una ma è per tenerle tutte
  group_by(week_hour_start, week_start, hour_start) %>%
  summarise(across(c(solare, batterie, `import - export`, produzione_rimanente,
                     load, solar_curtailment_mw_smooth, #  solar_curtailment_mw,
                     lmp_day_ahead_hourly, # lmp_day_ahead_hourly_smooth, 
                     lmp_real_time_15_min), # lmp_real_time_15_min_smooth),
                   ~ mean(.x, na.rm = TRUE)),
            .groups = 'drop')

glimpse(weekly_avg_data)

# ordinamento grafico
# ordinamento dei giorni in modo che la domenica sia l'ultimo e non il primo
# ed ordinamento del df per 'Date'
weekly_avg_data <- weekly_avg_data %>%
  mutate(week_start  = factor(week_start , levels = c("lunedì", "martedì", "mercoledì", "giovedì", "venerdì", "sabato", "domenica"))) %>%
  arrange(week_start)

# aggiunta di una riga per chiudere il periodo
weekly_avg_data <- weekly_avg_data %>%
  # usando add_row devo passare le colonne usando ".$"
  add_row(
    week_hour_start = "domenica 24:00:00",
    week_start = "domenica",
    hour_start = "24:00:00",
    solare = .$solare[1],
    batterie = .$batterie[1],
    `import - export` = .$`import - export`[1],
    produzione_rimanente = .$produzione_rimanente[1],
    load = .$load[1],
    # solar_curtailment_mw = .$solar_curtailment_mw[1],
    solar_curtailment_mw_smooth = .$solar_curtailment_mw_smooth[1],
    lmp_day_ahead_hourly = .$lmp_day_ahead_hourly[1],
    lmp_real_time_15_min = .$lmp_real_time_15_min[1]#,
    # lmp_day_ahead_hourly_smooth = .$lmp_day_ahead_hourly_smooth[1],
    # lmp_real_time_15_min_smooth = .$lmp_real_time_15_min_smooth[1]
  )

glimpse(weekly_avg_data)
tail(weekly_avg_data)
tail(weekly_avg_data$solar_curtailment_mw_smooth)
weekly_avg_data$solar_curtailment_mw_smooth[120:168]

write_csv(weekly_avg_data, "weekly_averaged_data.csv")
#####


