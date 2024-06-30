## codice per creare una visualizzazione del mix elettrico della rete californiana
# https://www.gridstatus.io/
# https://bpmcm.caiso.com/BPM%20Document%20Library/Market%20Instruments/BPM_for_Market%20Instruments_V85_Redline.pdf

## elaborazione grafica sui dati già scaricati e preparati

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


## definizione font e colori
#####
# google font: https://fonts.google.com/
font_testo <- "Lato"
font_titolo <- "Jost"

# Creazione dei colori
colori <- c("solare" = "#ffd700", "solare curtailment" = adjustcolor("#ffd700", alpha.f = 0.4), 
            "batterie" = "#f04fd9", "import - export" = "#c42912", "produzione rimanente" = "#808080", 
            "Load" = "black", "Day Ahead" = "#0000ff", "Real Time" = "#6be6ff")
lightgray <- "grey85"
midgray <- "grey75"
darkgray <- "gray40"
#####



##  lettura dati
settimana <- read_csv("weekly_averaged_data.csv")
glimpse(settimana)

settimana <- settimana %>%
  rename(
    `produzione rimanente` = produzione_rimanente,
    `solare curtailment` = solar_curtailment_mw_smooth
  )  %>% 
  mutate(
    # leggendo da excel devo specificare di nuovo che è un fattore
    week_hour_start = factor(week_hour_start, levels = unique(week_hour_start)),
    # conversione in GW per leggibilità
    across(c(solare, batterie, `import - export`, `produzione rimanente`, load, `solare curtailment`), ~ . / 1000)
  )

# selezione solo le colonne per l'area stacked
settimana_area <- settimana %>%
  select(week_hour_start, week_start, hour_start, solare, batterie, `import - export`, `produzione rimanente`, `solare curtailment`) %>%
  pivot_longer(
    cols = c(solare, batterie, `import - export`, `produzione rimanente`, `solare curtailment`),
    names_to = "category",
    values_to = "value"
  ) %>%
  filter(category %in% c("solare", "produzione rimanente", "batterie", "import - export", "solare curtailment")) %>%
  mutate(category = factor(category, levels = c("solare curtailment", "import - export", "batterie", "produzione rimanente", "solare")))
# mutate(category = factor(category, levels = c("solare curtailment", "import - export", "batterie", "solare", "produzione rimanente")))

glimpse(settimana_area)


# selezione solo delle colonne per le linee
settimana_line <- settimana %>%
  select(week_hour_start, week_start, hour_start, load, lmp_day_ahead_hourly, lmp_real_time_15_min)

glimpse(settimana_line)

##### grafico finale

# funzione per formattare le etichette dell'asse x
# mostra il nome del giorno solo a mezzogiorno e le ore in base all'intervallo specificato (6h di default)
format_x_labels <- function(interval = 6) {
  # in questo modo può accettare il parametro in input ed essere usata come format per l'asse x dei plot
  function(x) {
    # suddivisione della stringa per isolare l'ora ed il giorno
    split_values <- strsplit(x, " ")
    giorno <- sapply(split_values, function(v) v[1])
    ora_str <- sapply(split_values, function(v) v[2])
    # estrae solo l'ora da "HH:MM:SS"
    ora <- as.integer(sub(":.*", "", ora_str))
    # estrazione dei minuti in modo da stampare solo le etichette delle ore tonde
    minuti <- as.integer(sub(".*:(.*):.*", "\\1", ora_str))
    
    # gestione dell'ultima riga del dataset
    ora[ora == 24 & minuti == 0] <- 0
    # gestione dell'ultima riga del dataset nel caso di plot giorno singolo
    ora[ora == 25 & minuti == 0] <- 24
    
    # creazione etichette
    etichetta_ora <- ifelse(ora %% interval == 0 & minuti == 0, sprintf("%02d", ora), "")
    etichetta_giorno <- ifelse(ora == 12 & minuti == 0, paste("\n", giorno), "")
    # combinazione etichette con il giorno sotto l'ora a mezzogiorno
    ifelse(ora == 12 & minuti == 0, paste(etichetta_ora, etichetta_giorno, sep="\n"), etichetta_ora)
  }
}


# Calcolo dei tics per i major e minor grid
# 1 punto sono 5m -> 12 punti sono 1h
tics_major <- seq(1, nrow(settimana), by = 72)  # ogni 6 ore
tics_minor <- seq(1, nrow(settimana), by = 36)  # ogni 3 ore
# intervalli per segnare i diversi giorni, tolgo i due bordi estremi
tics_giorni  <- seq(1, nrow(settimana), by = 12*24)[-c(1,8)] 

# selezione limiti asse y

real_max_value_y <- settimana %>%
  # utilizo di pmax(.,0) per sommare solo i valori positivi (i negativi non fanno abbassare il grafico)
  mutate(
    total_sum = solare + `produzione rimanente` +
      pmax(batterie, 0, na.rm = TRUE) + 
      pmax(`import - export`, 0, na.rm = TRUE)
  ) %>%
  summarise(max_total_sum = max(total_sum, na.rm = TRUE)) %>%
  pull(max_total_sum)

max_value_y = ceiling(real_max_value_y / 5000/1000) * 5000/1000

# trovo il minimo sommando solo i valori negativi
real_min_value_y <- settimana %>%
  mutate(
    total_sum = solare + `produzione rimanente` +
      pmin(batterie, 0, na.rm = TRUE) +
      pmin(`import - export`, 0, na.rm = TRUE)
  ) %>%
  summarise(min_total_sum = - min(total_sum, na.rm = TRUE)) %>%
  pull(min_total_sum)

min_value_y = floor(real_min_value_y / 5000/1000) * 5000/1000

real_max_value_y_sec <- settimana %>%
  mutate(max = max(pmax(lmp_day_ahead_hourly, 0, na.rm = TRUE), pmax(lmp_real_time_15_min, 0, na.rm = TRUE))) %>%
  summarise(max_total = max(max, na.rm = TRUE)) %>%
  pull(max_total)

max_value_y_sec = ceiling(real_max_value_y_sec / 5) * 5

real_min_value_y_sec <- settimana %>%
  mutate(min = min(pmin(lmp_day_ahead_hourly, 0, na.rm = TRUE), pmin(lmp_real_time_15_min, 0, na.rm = TRUE))) %>%
  summarise(min_total = min(min, na.rm = TRUE)) %>%
  pull(min_total)

min_value_y_sec = floor(real_min_value_y_sec / 5) * 5 

# bisogna aggiunstare i 2 assi in modo che gli zeri combacino
# la distanza di max dallo 0 deve essere proporzionale a quello del min
print(c(max_value_y, min_value_y, max_value_y_sec, min_value_y_sec))
print(c(real_max_value_y, real_min_value_y, real_max_value_y_sec, real_min_value_y_sec))

rapp_max = max_value_y/max_value_y_sec
rapp_min =  min_value_y/min_value_y_sec
print(c(rapp_max, rapp_min))
real_rapp_max = real_max_value_y/real_max_value_y_sec
real_rapp_min =  real_min_value_y/real_min_value_y_sec
print(c(real_rapp_max, real_rapp_min))

# uso 500 come rapporto così sono anche proporzionali i tics
common_ratio <- 500/1000 # conversione per GW

# essendo 500/1000 > del max rapporto fra valori asse principale e secondario
# gli estremi del secondario scalati saranno più estremi degli estremi del principale
# ricalcolo i minimi e massimi
max_value_y <- ceiling(real_max_value_y_sec/5)*5*common_ratio
min_value_y <- floor(real_min_value_y_sec/5)*5*common_ratio


# linea per evidenziare il curltaiment solare
df_linea <- settimana %>%
  mutate(
    linea_curtailment_up = ifelse(`solare curtailment` > 0, 
                                  `produzione rimanente` + solare + `solare curtailment` + 
                                    pmax(batterie, 0, na.rm = TRUE) + pmax(`import - export`, 0, na.rm = TRUE), 
                                  NA),
    linea_curtailment_down = ifelse(`solare curtailment` > 0, 
                                    `produzione rimanente` + solare + 
                                      pmax(batterie, 0, na.rm = TRUE) + pmax(`import - export`, 0, na.rm = TRUE), 
                                    NA)
  ) %>%
  select(week_hour_start, linea_curtailment_up, linea_curtailment_down)

glimpse(df_linea)


# titolo e sottotitolo definiti qua per pulizia del codice
testo_titolo <- glue::glue(
  "California, Maggio 2024 | Produzione elettrica disaggregata per: ",
  '<b><span style = "color:{colori["solare"]}">solare</span></b>',
  ", ",
  '<b><span style = "color:{colori["batterie"]}">batterie</span></b>',
  ', ',
  '<b><span style = "color:{colori["import - export"]}">import - export</span></b>',
  ', ',
  '<b><span style = "color:{colori["produzione rimanente"]}">produzione rimanente</span></b>'
)

testo_sottotitolo <- glue::glue(
  "I valori sono calcolati facendo la media della produzione per fonte ",
  "per ogni specifico giorno della settimana ed intervallo di 5m"
)

legenda_prezzo <- glue::glue(
  "Prezzo Locale Marginale 15m tempo reale (asse dx) ",
  '<span style="font-style: italic; font-size: 7pt;">[TH_SP15_GEN-APND]</span>'
)

# creazione dei dati per le annotazioni
# la y è posizionata fuori dal range del grafico (in modo che stia sotto)

testo_spiegazione <- data.frame(
  # posizione tutta a sinistra
  x = -Inf,
  y = -39,
  labels = glue::glue(
    "<b>[*]</b> Con domanda si intende la quantità minima di ",
    "energia che deve circolare per mantenere la rete in attività, ",
    "ossia non sono considerati export e caricamento delle batterie."
  )
)

caption <- data.frame(
  # posizione tutta a destra
  x = Inf,
  y = -39,
  labels = "<b>Fonte:</b> CAISO e GRIDSTATUS | <b>Elaborazione:</b> Andrea Bellin"
)


# grafico con rappresentato il consumo medio della settimana per ora con mix stacked e load e prezzi giorno prima e real time
p_finale <- ggplot() +
  # griglia artificiale
  geom_hline(yintercept = seq(min_value_y, max_value_y, by = 5000/1000) , color = lightgray, linetype = "dotted") +
  # geom_hline(yintercept = seq(min_value_y, max_value_y, by = 1000) , color = midgray, linetype = "dotted", alpha = 0.3) +
  geom_vline(xintercept = tics_major , color = lightgray, linetype = "dotted") +
  geom_vline(xintercept = tics_minor , color = midgray, linetype = "dotted", alpha = 0.3) +
  # linee verticale per separare i giorni
  geom_vline(xintercept = tics_giorni, linetype = "solid", color = darkgray) +
  
  # aree impilate per la produzione (attenzione usare group = category altrimenti l'asse discreto non fa plottare)
  geom_area(data = settimana_area, aes(x = week_hour_start, y = value, fill = category, group = category), position = "stack", show.legend = FALSE) +
  geom_line(data = df_linea, aes(x = week_hour_start, y = linea_curtailment_up), group = 1, color = "black", linetype = "dashed", linewidth = 0.3, na.rm = TRUE) +
  geom_line(data = df_linea, aes(x = week_hour_start, y = linea_curtailment_down), group = 1, color = "black", linetype = "dashed", linewidth = 0.3, na.rm = TRUE) +
  
  # linee per il carico ed i prezzi
  geom_line(data = settimana_line, aes(x = week_hour_start, y = load, group = 1, colour = "Load"), linewidth = 1) +
  # geom_line(data = settimana_line, aes(x = week_hour_start, y = lmp_day_ahead_hourly * common_ratio, group = 1, colour = "Day Ahead"), linetype = "solid", linewidth = 1) +
  geom_line(data = settimana_line, aes(x = week_hour_start, y = lmp_real_time_15_min * common_ratio, group = 1, colour = "Day Ahead"), linetype = "solid", linewidth = 0.75) +
  
  ## definizione dei colori e personalizzazione della legenda
  scale_colour_manual(
    values = colori,
    name = "Con aggiunta delle linee per:",
    labels = c("Domanda (asse sx) [*]", legenda_prezzo),
    # colori di riferimento
    breaks = c("Load", "Day Ahead"), # "Real Time"
    guide = guide_legend(
      # per aumentare lo spessore delle linee nella legenda
      override.aes = list(linewidth = 1.5) 
    )
  ) +
  scale_fill_manual(values = colori) +
  scale_x_discrete(
    labels = format_x_labels(),
    breaks = settimana$week_hour_start[tics_major],
    minor_breaks = tics_minor,
    expand = c(0.005, 0.01)
  ) +
  scale_y_continuous(
    breaks = seq(min_value_y, max_value_y, by = 5000/1000), # conversione per GW
    minor_breaks = NULL, # seq(min_value_y, max_value_y, by = 1000),
    expand = c(0, 0),
    sec.axis = sec_axis(
      ~ . / common_ratio, name = "Prezzo ($/MWh)",
      # per i breaks uso i valori arrotondati ma per i limiti uso i valori aggiustati
      breaks = seq(min_value_y_sec, max_value_y_sec, by = 5) #,
      # guide = guide_axis(
      #   minor.ticks = TRUE
      # )
    )
  ) +
  theme_minimal(base_family = font_testo) +
  guides(
    # taglia la linea degli assi ai margini
    x = guide_axis(minor.ticks = TRUE, cap = "both"), 
    # ed aggiunge i tics più piccoli per l'asse y
    y = guide_axis(minor.ticks = TRUE, cap = "both")
  )  +
  theme(
    ## titolo e sottotitolo 
    plot.title = element_markdown(margin = margin(b = 5), size = 15.5, family = font_testo), # , face = "bold"
    plot.subtitle = element_markdown(margin = margin(b = 12), size = 12.5, family = font_testo),  
    
    ## legenda
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_markdown(size = 10),
    
    ## griglia grafico
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    # niente bordi per il grafico
    panel.border = element_blank(), 
    
    ## assi
    axis.line = element_line(color = "black", linewidth = 0.5),
    # spostamento titoli assi in alto (t) per l'asse x e destra (r) per l'asse y
    axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 10)), # , b = 0
    axis.title.y = element_text(size = 13, face = "bold", margin = margin(r = 5)),
    # tics
    axis.ticks = element_line(color = "black"),  
    axis.ticks.length = unit(0.25, "cm"),
    axis.minor.ticks.length = rel(0.5),
    # testo etichette sui tics
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 10),
    axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 10),
    # cambiamento colore delle etichette e titolo dell'asse y secondario
    axis.text.y.right = element_text(color = colori["Day Ahead"]), 
    axis.title.y.right = element_text(color = colori["Day Ahead"])
  ) +
  labs(
    x = "Giorno della settimana ed Ora",
    y = "Produzione (Area) e Domanda (Linea) [GW]",
    
    title = testo_titolo,
    
    subtitle = testo_sottotitolo,
  ) +
  # per non tagliare contenuti se fuori dal grafico
  # ylim serve definirlo dentro coord_cartesian() affinché funzioni il work around
  coord_cartesian(clip = "off", ylim = c(min_value_y, max_value_y))

p_finale_completo <- p_finale +
  # non è possibile aggiungere direttamente una doppia annotazione, questo è un work around
  # la posizione ed il testo è definito a parte sia per pulizia del codice che per passare l'argomento a 'data'
  geom_richtext(
    data = testo_spiegazione,
    aes(x = x, y = y, label = labels),
    hjust = 0, vjust = 1, fill = NA,
    label.color = NA, color = "black", size = 2.5, family = font_testo
  ) +
  geom_richtext(
    data = caption,
    aes(x = x, y = y, label = labels),
    hjust = 1, vjust = 1, fill = NA,
    label.color = NA, color = "black", size = 2, family = font_testo
  ) +
  # si espande il margine inferiore per farci stare il testo extra
  theme(plot.margin = margin(b = 14.5, t = 7, r = 5, l = 5, unit = 'pt'))
# plot(p_finale)

ggsave(
  filename = "caiso_settimana.jpg", plot = p_finale_completo,
  width = 12, height = 6, dpi = 1200
)



### da editare con canva

testo_spiegazione_canva <- data.frame(
  # posizione tutta a sinistra
  x = -Inf,
  y = -40,
  labels = glue::glue(
    "<b>[*]</b> Con domanda si intende la quantità minima di ",
    "energia che deve circolare per mantenere la rete in attività, ",
    "ossia non sono considerati export e caricamento delle batterie."
  )
)

caption_canva <- data.frame(
  # posizione tutta a destra
  x = Inf,
  y = -40,
  labels = "<b>Fonte:</b> CAISO e GRIDSTATUS | <b>Elaborazione:</b> Andrea Bellin"
)

p_finale_no_nitolo <- p_finale +
  geom_richtext(
    data = testo_spiegazione_canva,
    aes(x = x, y = y, label = labels),
    hjust = 0, vjust = 1, fill = NA,
    label.color = NA, color = "black", size = 2.5, family = font_testo
  ) +
  geom_richtext(
    data = caption_canva,
    aes(x = x, y = y, label = labels),
    hjust = 1, vjust = 1, fill = NA,
    label.color = NA, color = "black", size = 2, family = font_testo
  ) +
  ## adattamento dimensioni
  theme(
    plot.margin = margin(b = 16, t = 17, r = 5, l = 5, unit = 'pt'),
    ## titolo e sottotitolo 
    plot.title = element_markdown(margin = margin(b = 5), size = 15, family = font_testo),
    plot.subtitle = element_markdown(margin = margin(b = 12), size = 12, family = font_testo),  
  )


ggsave(
  filename = "caiso_settimana_no_titolo.jpg",plot = p_finale_no_nitolo,
  width = 12, height = 6, dpi = 1200
)




### tentativo di grafico con "baseload" (non è propriamente baseload ma è per intendersi)
# brutto anche perché c'è una piccola quota di solare tutto il giorno (anche usando l'applicativo di gridstatus)
#####
colori_baseload <- c("solare" = "#ffd700", "solare curtailment" = adjustcolor("#ffd700", alpha.f = 0.4), "batterie" = "#f04fd9", "import - export" = "#c42912", "produzione rimanente" = "#808080", "baseload" = "#808080", "Load" = "black", "Day Ahead" = "#0000ff", "Real Time" = "#6be6ff")

# calcolo del valore minimo  per produzione rimanente (= "baseload")
min_baseload <- min(settimana_area$value[settimana_area$category == "produzione rimanente"], na.rm = TRUE)

# creazione df dedicato
settimana_area_baseload <- settimana_area %>%
  mutate(
    # aggiornamento del valore per "produzione rimanente" togliendo il baseload
    value = ifelse(category == "produzione rimanente", value - min_baseload, value)
  ) %>%
  # agiunta righe per il baseload usando 'distinct' per evitare duplicati
  bind_rows(
    settimana_area %>%
      distinct(week_hour_start, week_start, hour_start) %>%
      mutate(
        category = "baseload",
        value = min_baseload
      )
  ) %>%
  mutate(category = factor(category, levels = c("solare curtailment", "import - export", "batterie", "produzione rimanente", "solare")))


glimpse(settimana_area_baseload)

# provo a separare la produzione rimanente in modo da mettere un baseload sotto ma far vedere il solare come se fosse sotto tutti
p_finale_baseload <- ggplot() +
  # griglia artificiale
  geom_hline(yintercept = seq(min_value_y, max_value_y, by = 5000/1000) , color = lightgray, linetype = "dotted") +
  # geom_hline(yintercept = seq(min_value_y, max_value_y, by = 1000) , color = midgray, linetype = "dotted", alpha = 0.3) +
  geom_vline(xintercept = tics_major , color = lightgray, linetype = "dotted") +
  geom_vline(xintercept = tics_minor , color = midgray, linetype = "dotted", alpha = 0.3) +
  # linee verticale per separare i giorni
  geom_vline(xintercept = tics_giorni, linetype = "solid", color = darkgray) +
  
  # aree impilate per la produzione (attenzione usare group = category altrimenti l'asse discreto non fa plottare)
  geom_area(data = settimana_area_baseload, aes(x = week_hour_start, y = value, fill = category, group = category), position = "stack", show.legend = FALSE) +
  geom_line(data = df_linea, aes(x = week_hour_start, y = linea_curtailment_up), group = 1, color = "black", linetype = "dashed", linewidth = 0.3, na.rm = TRUE) +
  geom_line(data = df_linea, aes(x = week_hour_start, y = linea_curtailment_down), group = 1, color = "black", linetype = "dashed", linewidth = 0.3, na.rm = TRUE) +
  
  # linee per il carico ed i prezzi
  geom_line(data = settimana_line, aes(x = week_hour_start, y = load, group = 1, colour = "Load"), linewidth = 1) +
  # geom_line(data = settimana_line, aes(x = week_hour_start, y = lmp_day_ahead_hourly * common_ratio, group = 1, colour = "Day Ahead"), linetype = "solid", linewidth = 1) +
  geom_line(data = settimana_line, aes(x = week_hour_start, y = lmp_real_time_15_min * common_ratio, group = 1, colour = "Day Ahead"), linetype = "solid", linewidth = 0.75) +
  
  ## definizione dei colori e personalizzazione della legenda
  scale_colour_manual(
    values = colori_baseload,
    name = "Con aggiunta delle linee per:",
    labels = c("Domanda (asse sx) [*]", "Prezzo 15m tempo reale (asse dx) [*]"), # , "Prezzo orario del giorno prima (asse dx)"
    # colori di riferimento
    breaks = c("Load", "Day Ahead") # , "Real Time"
    # guide = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid")))
  ) +
  scale_fill_manual(values = colori_baseload) +
  scale_x_discrete(
    labels = format_x_labels(),
    breaks = settimana$week_hour_start[tics_major],
    minor_breaks = tics_minor,
    expand = c(0.005, 0.01)
  ) +
  scale_y_continuous(
    breaks = seq(min_value_y, max_value_y, by = 5000/1000), # conversione per GW
    minor_breaks = NULL, # seq(min_value_y, max_value_y, by = 1000),
    expand = c(0, 0),
    sec.axis = sec_axis(
      ~ . / common_ratio, name = "Prezzo ($/MWh)",
      # per i breaks uso i valori arrotondati ma per i limiti uso i valori aggiustati
      breaks = seq(min_value_y_sec, max_value_y_sec, by = 5) #,
      # guide = guide_axis(
      #   minor.ticks = TRUE
      # )
    )
  ) +
  theme_minimal(base_family = font_testo) +
  guides(
    # taglia la linea degli assi ai margini
    x = guide_axis(minor.ticks = TRUE, cap = "both"), 
    # ed aggiunge i tics più piccoli per l'asse y
    y = guide_axis(minor.ticks = TRUE, cap = "both")
  )  +
  theme(
    axis.text.y.right = element_text(color = colori["Day Ahead"]), 
    axis.title.y.right = element_text(color = colori["Day Ahead"]),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.minor.ticks.length = rel(0.5),
    axis.title = element_text(size = 14, face = "bold"),
    panel.border = element_blank(),  # niente bordi
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black"),  
    axis.ticks.length = unit(0.25, "cm"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 8),
    axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 8),
    # spostamento titoli assi in alto (t) per l'asse x e destra (r) per l'asse y
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)), # , b = 0
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 5)),
    
    plot.title    = element_markdown( margin = margin(b = 5), size = 15.5, family = font_testo), # , face = "bold"
    plot.subtitle = element_markdown(margin = margin(b = 12), size = 12, family = font_testo), #  
  ) +
  labs(
    x = "Giorno della settimana ed Ora",
    y = "Produzione (Area) e Domanda (Linea) [GW]",
    
    title = testo_titolo,
    
    subtitle = testo_sottotitolo,
  ) +
  # non è possibile aggiungere direttamente una doppia annotazione, questo è un work around
  # la posizione ed il testo è definito a parte sia per pulizia del codice che per passare l'argomento a 'data'
  geom_richtext(
    data = testo_spiegazione,
    aes(x = x, y = y, label = labels),
    hjust = 0, vjust = 1, fill = NA,
    label.color = NA, color = "black", size = 2.5, family = font_testo
  ) +
  geom_richtext(
    data = caption,
    aes(x = x, y = y, label = labels),
    hjust = 1, vjust = 1, fill = NA,
    label.color = NA, color = "black", size = 2, family = font_testo
  ) +
  # si espande il margine inferiore per farci stare il testo extra
  theme(plot.margin = margin(b = 12, t = 7, r = 5, l = 5, unit = 'pt')) +
  # per non tagliare contenuti se fuori dal grafico
  # ylim serve definirlo dentro coord_cartesian() affinché funzioni il work around
  coord_cartesian(clip = "off", ylim = c(min_value_y - 0.5, max_value_y + 0.1))


# plot(p_finale)

ggsave(
  filename = "caiso_settimana_baseload.jpg",plot = p_finale_baseload,
  width = 12, height = 6, dpi = 600
)
#####