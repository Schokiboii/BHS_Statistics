library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(lubridate)
library(DT)
library(stringr)
library(ggplot2)

# Helper function to extract time strings as HH:MM:SS
extract_time_str <- function(time_col) {
  if (is.null(time_col) || all(is.na(time_col))) {
    return(rep(NA_character_, length(time_col)))
  }
  
  if (inherits(time_col, "POSIXt")) {
    return(format(time_col, "%H:%M:%S"))
  } else if (inherits(time_col, "difftime") || inherits(time_col, "hms")) {
    return(format(as.POSIXct(time_col), "%H:%M:%S"))
  } else {
    time_str <- as.character(time_col)
    time_str <- sub("^.*\\s+", "", time_str)
    return(time_str)
  }
}

# Helper function to convert time string HH:MM:SS to total minutes from midnight
time_to_mins <- function(time_str) {
  sapply(time_str, function(s) {
    if (is.na(s) || s == "" || s == "NA") return(NA_real_)
    parts <- unlist(strsplit(as.character(s), ":"))
    if (length(parts) >= 2) {
      hours <- as.numeric(parts[1])
      minutes <- as.numeric(parts[2])
      seconds <- if (length(parts) >= 3) as.numeric(parts[3]) else 0
      if (is.na(hours) || is.na(minutes)) return(NA_real_)
      return(hours * 60 + minutes + (seconds / 60))
    }
    return(NA_real_)
  })
}

# Helper function to calculate duration in minutes between two timestamps
calc_duration_mins <- function(start_time, end_time) {
  start_str <- extract_time_str(start_time)
  end_str <- extract_time_str(end_time)
  
  start_mins <- time_to_mins(start_str)
  end_mins <- time_to_mins(end_str)
  
  duration <- end_mins - start_mins
  duration <- ifelse(!is.na(duration) & duration < 0, duration + 1440, duration)
  return(round(duration, 1))
}

ui <- page_navbar(
  title = "Hermeskim Auswertung",
  
  # ---------------------------------------------------------------------------
  # TAB 1: Devices & Trips
  # ---------------------------------------------------------------------------
  nav_panel(
    title = "Geräte & Fahrten",
    layout_sidebar(
      sidebar = sidebar(
        fileInput("file1", "Excel-Datei hochladen (.xlsx / .xls)", accept = c(".xlsx", ".xls")),
        uiOutput("date_selector1")
      ),
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
  ),
  
  # ---------------------------------------------------------------------------
  # TAB 2: Evaluation A8 (Day-by-Day or All Days Analysis & Exclusion Filters)
  # ---------------------------------------------------------------------------
  nav_panel(
    title = "Auswertung A8",
    layout_sidebar(
      sidebar = sidebar(
        title = "A8 Filter & Einstellungen",
        uiOutput("a8_date_selector"),
        
        hr(),
        tags$b("Fahrten ausblenden:"),
        checkboxInput("hide_patra", "PATRA Aushilfe ausblenden", value = FALSE),
        checkboxInput("hide_op_assistance", "OP Aushilfe ausblenden", value = FALSE),
        checkboxInput("hide_laundry", "Wäsche ausblenden", value = FALSE),
        
        hr(),
        uiOutput("a8_ambulanz_filter"),
        
        accordion(
          open = FALSE,
          accordion_panel(
            "Andere Datei hochladen",
            fileInput("file_a8", "Datei auswählen (.xlsx)", accept = c(".xlsx", ".xls"))
          )
        )
      ),
      
      # Fixed height layout for KPI value boxes in Tab 2
      layout_columns(
        height = "160px",
        value_box(
          title = "Gesamte Einsätze",
          value = textOutput("a8_kpi_total")
        ),
        value_box(
          title = "Ø Bearbeitungsdauer",
          value = textOutput("a8_kpi_avg_time")
        ),
        value_box(
          title = "Top Ambulanz",
          value = textOutput("a8_kpi_top_amb")
        )
      ),
      
      navset_card_tab(
        nav_panel(
          "Grafische Auswertung",
          layout_columns(
            col_widths = c(12, 12, 12),
            fill = FALSE,
            card(
              fill = FALSE,
              card_header("Einsätze pro Ambulanz"),
              plotOutput("a8_plot_dept_counts")
            ),
            card(
              fill = FALSE,
              card_header("Ø Bearbeitungsdauer pro Ambulanz"),
              plotOutput("a8_plot_dept_duration")
            ),
            card(
              fill = FALSE,
              card_header("Tagesverlauf / Auslastung nach Uhrzeit"),
              plotOutput("a8_plot_hourly_distribution", height = "320px")
            )
          )
        ),
        nav_panel(
          "Zusammenfassung nach Ambulanz",
          DTOutput("a8_summary_table")
        ),
        nav_panel(
          "Detaildaten der Auswahl",
          DTOutput("a8_detail_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # ===========================================================================
  # SERVER LOGIC TAB 1: Devices & Trips
  # ===========================================================================
  raw_data1 <- reactive({
    req(input$file1)
    
    df <- read_excel(input$file1$datapath)
    df <- df %>% filter(!is.na(`Gerät`))
    
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
  
  output$date_selector1 <- renderUI({
    df <- raw_data1()
    available_dates <- sort(unique(df$Tag[!is.na(df$Tag)]))
    selectInput("selected_date1", "Tag auswählen:", choices = available_dates)
  })
  
  filtered_data1 <- reactive({
    req(input$selected_date1)
    df <- raw_data1()
    df %>% filter(Tag == as.Date(input$selected_date1))
  })
  
  output$total_trips <- renderText({
    nrow(filtered_data1())
  })
  
  output$unique_devices <- renderText({
    length(unique(filtered_data1()$`Gerät`))
  })
  
  output$accordion_devices <- renderUI({
    df <- filtered_data1()
    req(nrow(df) > 0)
    
    devices <- sort(unique(df$`Gerät`))
    
    panels <- lapply(devices, function(dev) {
      dev_df <- df %>% filter(`Gerät` == dev)
      
      avg_duration <- round(mean(dev_df$Transportdauer, na.rm = TRUE), 2)
      avg_punctuality <- round(mean(dev_df$Pünktlichkeit, na.rm = TRUE), 2)
      
      sitzend <- sum(grepl("sitzend", dev_df$Transport, ignore.case = TRUE), na.rm = TRUE)
      liegend <- sum(grepl("liegend", dev_df$Transport, ignore.case = TRUE), na.rm = TRUE)
      gehend  <- sum(grepl("gehend", dev_df$Transport, ignore.case = TRUE), na.rm = TRUE)
      eilig   <- sum(dev_df$`Priorität` == "Eilig", na.rm = TRUE)
      
      detail_table <- dev_df %>%
        transmute(
          Von = paste0(Von, " (", `Von Raum`, ")"),
          Nach = paste0(Nach, " (", `Nach Raum`, ")"),
          Transport,
          `Infektiös` = Infektiös,
          Dauer = Transportdauer,
          Pünktlichkeit
        )
      
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
  
  # ===========================================================================
  # SERVER LOGIC TAB 2: Evaluation A8
  # ===========================================================================
  raw_data_a8 <- reactive({
    file_path <- "../data/AuswertungA8.xlsx"
    
    req(!is.null(file_path), file.exists(file_path))
    
    df <- read_excel(file_path)
    
    df <- df %>%
      mutate(
        Datum = as.Date(Datum),
        Ambulanz = str_trim(as.character(Ambulanz)),
        Anmerkung = ifelse(is.na(Anmerkung), "", str_trim(as.character(Anmerkung))),
        Ambulanz = case_when(
          Ambulanz %in% c("Neurp", "Neuo") ~ "Neuro",
          Ambulanz == "Orto" ~ "Ortho",
          Ambulanz == "OP " ~ "OP",
          TRUE ~ Ambulanz
        ),
        Dauer_Minuten = calc_duration_mins(`Uhrzeit Annahme`, `Uhrzeit Abschluss`)
      )
    
    return(df)
  })
  
  # Date selector including "Alle Tage" option
  output$a8_date_selector <- renderUI({
    df <- raw_data_a8()
    available_dates <- sort(unique(df$Datum[!is.na(df$Datum)]))
    date_choices <- c("Alle Tage", as.character(available_dates))
    selectInput("a8_selected_date", "Tag auswählen:", choices = date_choices, selected = "Alle Tage")
  })
  
  output$a8_ambulanz_filter <- renderUI({
    df <- raw_data_a8()
    amb_choices <- c("Alle", sort(unique(df$Ambulanz)))
    selectInput("a8_selected_amb", "Gezielt nach Ambulanz filtern:", choices = amb_choices, selected = "Alle")
  })
  
  filtered_data_a8 <- reactive({
    req(input$a8_selected_date)
    df <- raw_data_a8()
    
    # Filter by specific date if "Alle Tage" is not selected
    if (input$a8_selected_date != "Alle Tage") {
      df <- df %>% filter(Datum == as.Date(input$a8_selected_date))
    }
    
    if (isTRUE(input$hide_patra)) {
      df <- df %>% filter(Ambulanz != "Patra")
    }
    
    if (isTRUE(input$hide_laundry)) {
      df <- df %>% filter(!grepl("Wäsche", Ambulanz, ignore.case = TRUE))
    }
    
    if (isTRUE(input$hide_op_assistance)) {
      df <- df %>% filter(!(Ambulanz == "OP" & grepl("Aushilfe", Anmerkung, ignore.case = TRUE)))
    }
    
    if (!is.null(input$a8_selected_amb) && input$a8_selected_amb != "Alle") {
      df <- df %>% filter(Ambulanz == input$a8_selected_amb)
    }
    
    return(df)
  })
  
  output$a8_kpi_total <- renderText({
    nrow(filtered_data_a8())
  })
  
  output$a8_kpi_avg_time <- renderText({
    df <- filtered_data_a8()
    if (nrow(df) == 0) return("0 min")
    avg_min <- round(mean(df$Dauer_Minuten, na.rm = TRUE), 1)
    if (is.nan(avg_min)) return("0 min")
    paste(avg_min, "min")
  })
  
  output$a8_kpi_top_amb <- renderText({
    df <- filtered_data_a8()
    if (nrow(df) == 0) return("-")
    top_amb <- df %>% 
      group_by(Ambulanz) %>% 
      tally() %>% 
      arrange(desc(n)) %>% 
      slice(1)
    
    paste0(top_amb$Ambulanz, " (", top_amb$n, ")")
  })
  
  # Plot 1: Trip counts per department with tighter row spacing
  output$a8_plot_dept_counts <- renderPlot({
    df <- filtered_data_a8()
    req(nrow(df) > 0)
    
    dept_summary <- df %>%
      mutate(Ambulanz = as.character(Ambulanz)) %>%
      count(Ambulanz) %>%
      filter(n > 0) %>%
      arrange(n)
    
    ggplot(dept_summary, aes(x = reorder(Ambulanz, n), y = n)) +
      geom_col(fill = "#2b5c8f", width = 0.7) +
      geom_text(aes(label = n), hjust = -0.2, size = 3.6) +
      coord_flip() +
      scale_x_discrete(drop = TRUE) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = NULL, y = "Anzahl Einsätze") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 9.5, color = "#222222"),
        axis.text.x = element_text(color = "#333333")
      )
  }, height = function() {
    df <- filtered_data_a8()
    n_depts <- length(unique(df$Ambulanz[df$Ambulanz != ""]))
    max(220, n_depts * 18)
  })
  
  # Plot 2: Average duration per department with tighter row spacing
  output$a8_plot_dept_duration <- renderPlot({
    df <- filtered_data_a8()
    req(nrow(df) > 0)
    
    dept_duration <- df %>%
      mutate(Ambulanz = as.character(Ambulanz)) %>%
      group_by(Ambulanz) %>%
      summarise(avg_dur = mean(Dauer_Minuten, na.rm = TRUE), n = n(), .groups = "drop") %>%
      filter(n > 0, !is.na(avg_dur)) %>%
      arrange(avg_dur)
    
    ggplot(dept_duration, aes(x = reorder(Ambulanz, avg_dur), y = avg_dur)) +
      geom_col(fill = "#d9534f", width = 0.7) +
      geom_text(aes(label = sprintf("%.1f min", avg_dur)), hjust = -0.15, size = 3.6) +
      coord_flip() +
      scale_x_discrete(drop = TRUE) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      labs(x = NULL, y = "Ø Dauer (Minuten)") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 9.5, color = "#222222"),
        axis.text.x = element_text(color = "#333333")
      )
  }, height = function() {
    df <- filtered_data_a8()
    n_depts <- length(unique(df$Ambulanz[df$Ambulanz != ""]))
    max(220, n_depts * 18)
  })
  
  # Plot 3: Hourly trip distribution throughout the day
  output$a8_plot_hourly_distribution <- renderPlot({
    df <- filtered_data_a8()
    req(nrow(df) > 0)
    
    hourly_df <- df %>%
      mutate(hour_val = as.numeric(sub(":.*", "", extract_time_str(`Uhrzeit Annahme`)))) %>%
      filter(!is.na(hour_val)) %>%
      count(hour_val)
    
    ggplot(hourly_df, aes(x = hour_val, y = n)) +
      geom_col(fill = "#207382", alpha = 0.85, width = 0.65) +
      geom_line(group = 1, color = "#0f3a47", linewidth = 1) +
      geom_point(color = "#0f3a47", size = 2.5) +
      scale_x_continuous(breaks = seq(0, 23, by = 1)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = "Uhrzeit (Stunden)", y = "Anzahl Anfragen") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(color = "#333333")
      )
  })
  
  output$a8_summary_table <- renderDT({
    df <- filtered_data_a8()
    req(nrow(df) > 0)
    
    summary_df <- df %>%
      group_by(Ambulanz) %>%
      summarise(
        `Anzahl Einsätze` = n(),
        `Ø Dauer (min)` = round(mean(Dauer_Minuten, na.rm = TRUE), 1),
        `Min Dauer (min)` = round(min(Dauer_Minuten, na.rm = TRUE), 1),
        `Max Dauer (min)` = round(max(Dauer_Minuten, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      arrange(desc(`Anzahl Einsätze`))
    
    datatable(
      summary_df,
      options = list(pageLength = 10, dom = 'ftp', autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  output$a8_detail_table <- renderDT({
    df <- filtered_data_a8()
    req(nrow(df) > 0)
    
    detail_df <- df %>%
      select(
        Datum, 
        `Uhrzeit Annahme`, 
        `Uhrzeit Abschluss`, 
        `Dauer (min)` = Dauer_Minuten, 
        Ambulanz, 
        Anmerkung
      ) %>%
      mutate(
        Datum = format(Datum, "%d.%m.%Y"),
        `Uhrzeit Annahme` = extract_time_str(`Uhrzeit Annahme`),
        `Uhrzeit Abschluss` = extract_time_str(`Uhrzeit Abschluss`)
      )
    
    datatable(
      detail_df,
      options = list(pageLength = 15, dom = 'ftp', autoWidth = TRUE),
      rownames = FALSE
    )
  })
}

shinyApp(ui = ui, server = server)