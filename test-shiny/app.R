library(shiny)
library(ggplot2)

# Funktion, um Daten aus einer Datei zu laden und zu verarbeiten. Die Spalten der Datei werden spezifiziert, die UTC-Zeit wird extrahiert, 
# und die Stunden sowie das Datum aus den Zeitstempeln werden berechnet.
load_data <- function(file_path) {
  header <- c("UTC Date and Time", "Local Date and Time", "Enclosure Temperature", "Sky Temperature", "Frequency", "MSAS", "ZP", "Sequence Number")
  data <- read.delim(file_path, skip = 35, sep = ";", header = FALSE, col.names = header)
  
  # Zerlegung der "UTC Date and Time"-Spalte in Datum und Zeit, Speicherung als Zeichenkette.
  DateTime_UTC <- lapply(strsplit(data$UTC.Date.and.Time, "T"), as.character)
  
  # Umwandlung des extrahierten Datums in das Datum-Format, Formatierung der Zeit als POSIXct.
  data$UTC.Date <- as.Date(sapply(DateTime_UTC, "[[", 1), format = "%Y-%m-%d")
  data$UTC.Time <- as.POSIXct(paste(sapply(DateTime_UTC, "[[", 1), sapply(DateTime_UTC, "[[", 2)), format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  
  # Extraktion der Stunde aus dem Zeitstempel und Konvertierung in numerisches Format.
  data$Hour <- as.numeric(format(data$UTC.Time, "%H"))
  data$Date <- as.Date(data$UTC.Time)
  
  return(data)
}

# Auflistung und Ermittlung der Dateipfade für alle relevanten Datendateien im Ordner.
files <- list.files(path = "../data/", pattern = "stars927_2024-.*\\.dat", full.names = TRUE)

# Zusammenführung der geladenen Daten aus allen Dateien in einen einzigen Datensatz mittels "rbind".
stars_data <- do.call(rbind, lapply(files, load_data))

# UI-Elemente für die Shiny-App. Erstellung eines Date-Range-Inputs und eines Plot-Bereichs.----------------------------------------------------------
ui <- fluidPage(
  titlePanel("Photometer Daten Analyse"),
  
  sidebarLayout(
    sidebarPanel(
      # Kompakteres Layout: Die Eingabefelder werden untereinander angeordnet
      tags$div(
        style = "display: flex; flex-direction: column; gap: 10px;",
        dateRangeInput("dateRange", "Wähle Datum:", start = min(stars_data$UTC.Date), end = max(stars_data$UTC.Date))
      )
    ),
    
    mainPanel(
      # Grafikbereich wird in der Höhe gestreckt
      plotOutput("magnitudePlot", height = "700px")  # Erhöht die Höhe des Plots
    )
  )
)

# Server-Logik für die Shiny-App.--------------------------------------------------------------------------------------------------------------------
server <- function(input, output) {
  output$magnitudePlot <- renderPlot({
    # Filterung der Daten basierend auf dem vom Benutzer ausgewählten Datumsbereich.
    filtered_data <- subset(stars_data, UTC.Date >= input$dateRange[1] & UTC.Date <= input$dateRange[2])
    
    # Auswahl der Daten für die Nachtstunden (21:00 bis 04:00 Uhr) und Berechnung der Durchschnittstemperatur pro Nacht.
    night_data <- subset(filtered_data, Hour >= 21 | Hour <= 4)
    avg_temp_night <- aggregate(Sky.Temperature ~ Date, night_data, mean)
    
    # Auswahl der Nächte mit einer Durchschnittstemperatur unter 0°C.
    cold_nights <- subset(avg_temp_night, Sky.Temperature < 0)$Date
    cold_night_data <- subset(night_data, Date %in% cold_nights)
    
    # Berechnung des maximalen MSAS-Werts für jede kalte Nacht.
    max_magnitude_nights <- aggregate(MSAS ~ Date, cold_night_data, max)
    
    # Zusammenführung der Daten, um pro Nacht nur den höchsten MSAS-Wert zu erhalten.
    plot_data <- merge(cold_night_data, max_magnitude_nights, by = c("Date", "MSAS"))
    plot_data <- plot_data[!duplicated(plot_data$Date), ]
    
    # Definition der visuellen Bortle-Skalenwerte für den Hintergrund des Plots.
    bortle_levels <- data.frame(
      ymin = c(21.90, 21.50, 21.30, 20.80, 20.10, 19.10, 18.00, 17.5),
      ymax = c(22.5, 21.90, 21.50, 21.30, 20.80, 20.10, 19.10, 18.0),
      fill = factor(c("Bortle 1", "Bortle 2", "Bortle 3", "Bortle 4", "Bortle 5", "Bortle 6", "Bortle 7", "Bortle 8"))
    )
    
    # Erstellen eines Plots, der die maximalen MSAS-Werte in kalten Nächten anzeigt, inklusive Farbskalierung basierend auf der Himmels-Temperatur.
    ggplot() +
      # Bortle-Skala als Hintergrund hinzufügen mit Legende.
      geom_rect(data = bortle_levels, aes(ymin = ymin, ymax = ymax, xmin = -Inf, xmax = Inf, fill = fill), alpha = 0.1) +
      geom_hline(yintercept = 21.3, color = "red", linetype = "dashed") +
      
      # Punkte werden nach Temperatur angezeigt.
      geom_point(data = plot_data, size = 3, aes(x = format(UTC.Time, "%Y-%m-%d"), y = MSAS, color = Sky.Temperature)) +
      geom_text(data = plot_data, aes(x = format(UTC.Time, "%Y-%m-%d"), y = MSAS, label = round(Sky.Temperature, 2)), vjust = -1, size = 3) +
      
      scale_y_continuous(limits = c(17.5, 22.5)) +
      
      # Farbskala für die Temperaturpunkte (entsprechend den geforderten Farben).
      scale_color_gradientn(
        colors = c("#132B43", "#56B1F7", "white"),  # Blau für kältere, Weiß für wärmere Temperaturen
        values = scales::rescale(c(-10, 0, 10)),  # Wertebereich für die Farben
        name = "Sky Temperature [°C]"  # Name der Legende
      ) +
      
      # Legende für die Bortle-Skala.
      scale_fill_manual(
        values = c("Bortle 1" = "black", "Bortle 2" = "grey20", "Bortle 3" = "darkblue", "Bortle 4" = "darkgreen", 
                   "Bortle 5" = "yellow", "Bortle 6" = "orange", "Bortle 7" = "red", "Bortle 8" = "white"),
        name = "Bortle Scale"
      ) +
      
      labs(
        title = "Maximale MSAS Werte für Nächte mit Durchschnitts-Temperatur unter 0°C",
        x = "UTC Time",
        y = "MSAS [ mag/arcsec² ]",
        color = "Sky Temperature [°C]"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
}

# Start der Shiny-App und Aktivierung der definierten Benutzeroberfläche und Server-Logik.
shinyApp(ui = ui, server = server)
