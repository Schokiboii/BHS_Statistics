library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(lubridate)
library(DT)

ui <- page_sidebar(
  title = "Hermeskim Auswertung – Geräte & Fahrten",
  
  sidebar = sidebar(
    fileInput("file", "Excel-Datei hochladen (.xlsx / .xls)", accept = c(".xlsx", ".xls")),
    uiOutput("date_selector")
  ),
  
  # KPI-Karten oben mit fester Höhe
  layout_columns(
    height = "300px",
    value_box(
      title = "Gesamte Fahrten mit Gerät (Tag)",
      value = textOutput("total_trips")
    ),
    value_box(
      title = "Eingesetzte Geräte",
      value = textOutput("unique_devices")
    )
  ),
  
  card(
    card_header("Auswertung pro Gerät"),
    uiOutput("accordion_devices")
  )
)

server <- function(input, output, session) {
  
  # Datensatz einlesen und aufbereiten
  raw_data <- reactive({
    req(input$file)
    
    df <- read_excel(input$file$datapath)
    
    # Zeilen ohne Gerät ignorieren
    df <- df %>% filter(!is.na(`Gerät`))
    
    # Werte explizit in numerische Datentypen umwandeln & Datum parsen
    df <- df %>%
      mutate(
        Transportdauer = as.numeric(gsub(",", ".", `Transportdauer`)),
        Pünktlichkeit = as.numeric(gsub(",", ".", `Pünktlichkeit`)),
        Datum_Obj = parse_date_time(`Plan-Abholung`, orders = c("d.m.y H:M", "d.m.Y H:M")),
        Tag = as.Date(Datum_Obj),
        `Gerät` = as.character(`Gerät`)
      )
    
    return(df)
  })
  
  # Dynamisches Dropdown für Tage aus den vorhandenen Daten
  output$date_selector <- renderUI({
    df <- raw_data()
    available_dates <- sort(unique(df$Tag[!is.na(df$Tag)]))
    selectInput("selected_date", "Tag auswählen:", choices = available_dates)
  })
  
  # Nach ausgewähltem Tag filtern
  filtered_data <- reactive({
    req(input$selected_date)
    df <- raw_data()
    df %>% filter(Tag == as.Date(input$selected_date))
  })
  
  # KPI 1: Gesamte Fahrten am ausgewählten Tag
  output$total_trips <- renderText({
    nrow(filtered_data())
  })
  
  # KPI 2: Anzahl unterschiedlicher Geräte am Tag
  output$unique_devices <- renderText({
    length(unique(filtered_data()$`Gerät`))
  })
  
  # Dynamisches Erstellen der Accordion-Panels pro Gerät
  output$accordion_devices <- renderUI({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    devices <- sort(unique(df$`Gerät`))
    
    panels <- lapply(devices, function(dev) {
      dev_df <- df %>% filter(`Gerät` == dev)
      
      avg_duration <- round(mean(dev_df$Transportdauer, na.rm = TRUE), 2)
      avg_punctuality <- round(mean(dev_df$Pünktlichkeit, na.rm = TRUE), 2)
      
      # Gerätespezifische Werte berechnen:
      sitzend <- sum(grepl("sitzend", dev_df$Transport, ignore.case = TRUE), na.rm = TRUE)
      liegend <- sum(grepl("liegend", dev_df$Transport, ignore.case = TRUE), na.rm = TRUE)
      gehend  <- sum(grepl("gehend", dev_df$Transport, ignore.case = TRUE), na.rm = TRUE)
      eilig   <- sum(dev_df$`Priorität` == "Eilig", na.rm = TRUE)
      
      # Spaltenaufbereitung für die Detailtabelle
      detail_table <- dev_df %>%
        transmute(
          Von = paste0(Von, " (", `Von Raum`, ")"),
          Nach = paste0(Nach, " (", `Nach Raum`, ")"),
          Transport,
          `Infektiös` = Infektiös,
          Dauer = Transportdauer,
          Pünktlichkeit
        )
      
      # Neuer Titel für den ausklappbaren Bereich pro Gerät
      accordion_panel(
        title = HTML(sprintf(
          "<b>%s</b> | Ø Dauer: %s min | Ø Pünktlichkeit: %s | Fahrten: %d <br> S: %d / L: %d / G: %d | Eilig: %d",
          dev, avg_duration, avg_punctuality, nrow(dev_df), sitzend, liegend, gehend, eilig
        )),
        renderDT({
          datatable(
            detail_table, 
            options = list(pageLength = 10, dom = 'tp', autoWidth = TRUE),
            rownames = FALSE
          )
        })
      )
    })
    
    do.call(accordion, c(panels, list(id = "device_accordion", multiple = TRUE)))
  })
}

shinyApp(ui = ui, server = server)