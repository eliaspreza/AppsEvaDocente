library(shiny)
library(bslib)
library(leaflet)
library(DT)
library(tidyverse)
library(janitor)
library(curl)
library(readr)

# Kobo API CSV URL
kobo_url <- "https://kf.kobotoolbox.org/api/v2/assets/arhBtsGHf5zYZbxpPUUaxv/export-settings/esfNotcMyUiRfdFwSSNfKR4/data.csv"

# Target constants for progress calculations
TARGET_TOTAL <- 2736
TARGET_CONTROL <- 1042
TARGET_T1 <- 847
TARGET_T2 <- 847

# Function to fetch and clean data from Kobo Toolbox API
fetch_clean_kobo <- function() {
  message("[API] Intentando conectar con la API de KoboToolbox...")
  tryCatch({
    # Fetch CSV data from Kobo API using semicolon delimiter
    df_raw <- read_delim(kobo_url, delim = ";", show_col_types = FALSE)
    
    # 2) Limpiar los nombres de las columnas con janitor
    df_clean <- df_raw %>% janitor::clean_names()
    
    # 3) Mostrar la base o diccionario de columnas en la consola usando glimpse
    message("\n--- VERIFICACIÓN DE DICCIONARIO DE COLUMNAS (GLIMPSE) ---")
    glimpse(df_clean[, 1:min(20, ncol(df_clean))])
    message("----------------------------------------------------------\n")
    
    # Ensure date columns are formatted correctly
    if ("fecha_en_que_se_desarrollo_la_clase" %in% colnames(df_clean)) {
      df_clean$fecha_en_que_se_desarrollo_la_clase <- as.Date(df_clean$fecha_en_que_se_desarrollo_la_clase)
    } else {
      df_clean$fecha_en_que_se_desarrollo_la_clase <- Sys.Date()
    }
    
    if ("submission_time" %in% colnames(df_clean)) {
      df_clean$submission_time <- as.POSIXct(df_clean$submission_time)
    } else {
      df_clean$submission_time <- Sys.time()
    }
    
    # Ensure coordinates are numeric
    if ("coordenadas_gps_del_centro_educativo_latitude" %in% colnames(df_clean)) {
      df_clean$coordenadas_gps_del_centro_educativo_latitude <- as.numeric(df_clean$coordenadas_gps_del_centro_educativo_latitude)
    }
    if ("coordenadas_gps_del_centro_educativo_longitude" %in% colnames(df_clean)) {
      df_clean$coordenadas_gps_del_centro_educativo_longitude <- as.numeric(df_clean$coordenadas_gps_del_centro_educativo_longitude)
    }
    
    # Specific correction for survey 20 (Centro Escolar Jardines de Monte Blanco)
    # and global validation inside El Salvador bounding box: Lat [13.0, 14.6], Lon [-90.3, -87.3]
    if ("coordenadas_gps_del_centro_educativo_latitude" %in% colnames(df_clean) && 
        "coordenadas_gps_del_centro_educativo_longitude" %in% colnames(df_clean)) {
      
      df_clean <- df_clean %>%
        mutate(
          coordenadas_gps_del_centro_educativo_latitude = ifelse(index == 20, 13.71587, coordenadas_gps_del_centro_educativo_latitude),
          coordenadas_gps_del_centro_educativo_longitude = ifelse(index == 20, -89.12041, coordenadas_gps_del_centro_educativo_longitude)
        ) %>%
        mutate(
          coordenadas_gps_del_centro_educativo_latitude = ifelse(
            coordenadas_gps_del_centro_educativo_latitude >= 13.0 & 
            coordenadas_gps_del_centro_educativo_latitude <= 14.6,
            coordenadas_gps_del_centro_educativo_latitude,
            NA_real_
          ),
          coordenadas_gps_del_centro_educativo_longitude = ifelse(
            coordenadas_gps_del_centro_educativo_longitude >= -90.3 & 
            coordenadas_gps_del_centro_educativo_longitude <= -87.3,
            coordenadas_gps_del_centro_educativo_longitude,
            NA_real_
          )
        )
    }
    
    # Fill groups T missing values (just in case)
    if (!"grupos_t_oculto" %in% colnames(df_clean)) {
      df_clean$grupos_t_oculto <- "Control"
    } else {
      df_clean$grupos_t_oculto <- ifelse(is.na(df_clean$grupos_t_oculto) | df_clean$grupos_t_oculto == "", "Control", df_clean$grupos_t_oculto)
    }
    
    # Fill sex of teachers missing values
    if (!"sexo_del_docente" %in% colnames(df_clean)) {
      df_clean$sexo_del_docente <- "Femenino"
    } else {
      df_clean$sexo_del_docente <- ifelse(is.na(df_clean$sexo_del_docente) | df_clean$sexo_del_docente == "", "Femenino", df_clean$sexo_del_docente)
    }
    
    # Save a local backup copy in the app directory (handled for read-only environments)
    tryCatch({
      saveRDS(df_clean, "local_backup.rds")
    }, error = function(err) {
      warning("[BACKUP ERROR] No se pudo guardar 'local_backup.rds' (ambiente de solo lectura): ", err$message)
    })
    return(df_clean)
  }, error = function(e) {
    warning("[API ERROR] Error al descargar de Kobo: ", e$message)
    # Check if a local backup exists
    if (file.exists("local_backup.rds")) {
      message("[API BACKUP] Cargando datos desde copia de seguridad local...")
      return(readRDS("local_backup.rds"))
    } else {
      # Return a default empty tibble structure if backup doesn't exist
      message("[API BACKUP] No se encontró copia de seguridad local. Retornando estructura vacía...")
      return(tibble(
        index = numeric(),
        submission_time = as.POSIXct(character()),
        fecha_en_que_se_desarrollo_la_clase = as.Date(character()),
        nombre_del_centro_educativo = character(),
        zona = character(),
        departamento = character(),
        distrito = character(),
        cluster_oculto = numeric(),
        grupos_t_oculto = character(),
        id_observador = numeric(),
        coordenadas_gps_del_centro_educativo_latitude = numeric(),
        coordenadas_gps_del_centro_educativo_longitude = numeric(),
        sexo_del_docente = character()
      ))
    }
  })
}

# ----------------- UI DESIGN -----------------
ui <- page_sidebar(
  # Clean, professional clear/light theme using bslib
  theme = bs_theme(
    version = 5,
    bootswatch = "cosmo", 
    primary = "#2563eb",  # Premium royal blue
    secondary = "#64748b", # slate-500
    success = "#10b981",  # emerald-500
    info = "#0ea5e9",     # sky-500
    warning = "#f59e0b",  # amber-500
    danger = "#ef4444"    # red-500
  ),
  
  # Top Navigation / Title Area
  title = div(
    style = "display: flex; align-items: center; justify-content: space-between; width: 100%; padding: 4px 10px;",
    div(
      h3("Monitoreo del Levantamiento en Campo — EVA Teach", style = "margin: 0; font-weight: 700; color: #ffffff; font-size: 22px;"),
      p("Información extraída en tiempo real de KoboToolbox - Dashboard por EUROLATINA", style = "margin: 0; font-size: 13px; color: #f1f5f9; font-weight: 500;")
    ),
    div(
      style = "display: flex; align-items: center; gap: 15px;",
      span(textOutput("last_update_text"), style = "font-size: 12px; color: #f1f5f9; font-style: italic; font-weight: 500;"),
      actionButton("refresh_btn", "Actualizar API", icon = icon("rotate"), class = "btn-light btn-sm", style = "font-weight: 600; border-radius: 8px; color: #2563eb;")
    )
  ),
  
  # 8) Sidebar on the left containing responsive filters
  sidebar = sidebar(
    title = span(icon("filter"), "Filtros de Control", style = "font-weight: 700; color: #1e3a8a; font-size: 16px;"),
    width = 320,
    bg = "#f8fafc", # Clean light slate background
    
    selectizeInput(
      inputId = "filter_depto",
      label = "Departamento:",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = 'Todos los departamentos', plugins = list('remove_button'))
    ),
    
    selectizeInput(
      inputId = "filter_distrito",
      label = "Distrito:",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = 'Todos los distritos', plugins = list('remove_button'))
    ),
    
    selectizeInput(
      inputId = "filter_zona",
      label = "Zona:",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = 'Todas las zonas', plugins = list('remove_button'))
    ),
    
    selectizeInput(
      inputId = "filter_grupo",
      label = "Grupo de Tratamiento (Grupo T):",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = 'Todos los grupos', plugins = list('remove_button'))
    ),
    
    selectizeInput(
      inputId = "filter_obs",
      label = "ID Observador (Encuestador):",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = 'Todos los observadores', plugins = list('remove_button'))
    ),
    
    dateRangeInput(
      inputId = "filter_date",
      label = "Rango de Fecha de Levantamiento:",
      start = NULL,
      end = NULL,
      format = "dd/mm/yyyy",
      separator = " a ",
      language = "es"
    ),
    
    hr(style = "border-color: #cbd5e1; margin: 15px 0;"),
    actionButton(
      inputId = "clear_filters_btn",
      label = "Limpiar Filtros",
      icon = icon("filter-circle-xmark"),
      class = "btn-outline-secondary w-100",
      style = "font-weight: 600; border-radius: 8px; font-size: 13px;"
    )
  ),
  
  # Custom CSS and Styling
  tags$head(
    tags$style(HTML("
      /* Page Background */
      body {
        background-color: #f1f5f9 !important;
      }
      
      /* Value Box Gradient Styling - Rich Aesthetics */
      .bg-gradient-total {
        background: linear-gradient(135deg, #1e3a8a, #2563eb) !important;
        color: white !important;
        box-shadow: 0 4px 10px rgba(37, 99, 235, 0.15) !important;
        border: none !important;
        border-radius: 12px !important;
        transition: transform 0.2s, box-shadow 0.2s;
      }
      .bg-gradient-control {
        background: linear-gradient(135deg, #581c87, #7c3aed) !important;
        color: white !important;
        box-shadow: 0 4px 10px rgba(124, 58, 237, 0.15) !important;
        border: none !important;
        border-radius: 12px !important;
        transition: transform 0.2s, box-shadow 0.2s;
      }
      .bg-gradient-t1 {
        background: linear-gradient(135deg, #0f766e, #0d9488) !important;
        color: white !important;
        box-shadow: 0 4px 10px rgba(13, 148, 136, 0.15) !important;
        border: none !important;
        border-radius: 12px !important;
        transition: transform 0.2s, box-shadow 0.2s;
      }
      .bg-gradient-t2 {
        background: linear-gradient(135deg, #9d174d, #db2777) !important;
        color: white !important;
        box-shadow: 0 4px 10px rgba(219, 39, 119, 0.15) !important;
        border: none !important;
        border-radius: 12px !important;
        transition: transform 0.2s, box-shadow 0.2s;
      }
      .bg-gradient-schools {
        background: linear-gradient(135deg, #854d0e, #d97706) !important;
        color: white !important;
        box-shadow: 0 4px 10px rgba(217, 119, 6, 0.15) !important;
        border: none !important;
        border-radius: 12px !important;
        transition: transform 0.2s, box-shadow 0.2s;
      }
      .bg-gradient-total:hover, .bg-gradient-control:hover, .bg-gradient-t1:hover, .bg-gradient-t2:hover, .bg-gradient-schools:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 15px rgba(0,0,0,0.1) !important;
      }
      
      /* Value Box Size Reduction & Text Enlarge */
      .value-box {
        min-height: 70px !important;
        height: 75px !important;
        padding: 4px 10px !important;
      }
      .value-box .value-box-area {
        gap: 1px !important;
        display: flex !important;
        flex-direction: column !important;
        justify-content: center !important;
        padding: 0 !important;
        margin: 0 !important;
      }
      .value-box-title {
        font-size: 12px !important;
        font-weight: 800 !important;
        text-transform: uppercase !important;
        letter-spacing: 0.03em !important;
        margin-bottom: 0px !important;
        opacity: 0.95 !important;
        line-height: 1.1 !important;
      }
      .value-box-value {
        font-size: 28px !important;
        font-weight: 900 !important;
        line-height: 1.0 !important;
        margin-bottom: 0px !important;
        margin-top: 1px !important;
      }
      
      /* Value Box Subtitles */
      .value-box p {
        font-size: 10.5px !important;
        margin: 0 !important;
        opacity: 0.9 !important;
        line-height: 1.1 !important;
        margin-top: 1px !important;
        font-weight: 600 !important;
      }
      
      .value-box .showcase {
        font-size: 20px !important;
        opacity: 0.18 !important;
        margin-right: 2px !important;
      }
      
      /* Card Wrapper */
      .card {
        border-radius: 12px !important;
        border: 1px solid #e2e8f0 !important;
        box-shadow: 0 4px 6px -1px rgb(0 0 0 / 0.05), 0 2px 4px -2px rgb(0 0 0 / 0.05) !important;
      }
      
      /* Tabs Styling */
      .nav-tabs {
        background-color: #f8fafc !important;
        border-bottom: 1px solid #e2e8f0 !important;
        border-top-left-radius: 12px !important;
        border-top-right-radius: 12px !important;
        padding: 6px 12px 0 12px !important;
      }
      .nav-link {
        font-weight: 600 !important;
        color: #475569 !important;
        border: none !important;
        padding: 10px 18px !important;
        font-size: 14px !important;
        border-radius: 0 !important;
      }
      .nav-link.active {
        color: #2563eb !important;
        background-color: transparent !important;
        border-bottom: 3px solid #2563eb !important;
        font-weight: 700 !important;
      }
      
      /* DT buttons styling */
      .dt-buttons {
        margin-bottom: 15px !important;
      }
      .dt-buttons .btn {
        background-color: #ffffff !important;
        border: 1px solid #cbd5e1 !important;
        color: #334155 !important;
        font-weight: 600 !important;
        padding: 5px 12px !important;
        font-size: 12.5px !important;
        border-radius: 6px !important;
        transition: all 0.15s;
        margin-right: 5px;
      }
      .dt-buttons .btn:hover {
        background-color: #f8fafc !important;
        border-color: #94a3b8 !important;
        color: #0f172a !important;
      }
      
      /* DT Table style overrides */
      table.dataTable {
        border-collapse: collapse !important;
        font-size: 13.5px !important;
      }
      table.dataTable thead th {
        background-color: #f8fafc !important;
        color: #1e293b !important;
        font-weight: 600 !important;
        border-bottom: 2px solid #e2e8f0 !important;
        padding: 10px !important;
      }
      table.dataTable tbody td {
        padding: 8px 10px !important;
        border-bottom: 1px solid #f1f5f9 !important;
      }
      
      /* Leaflet control style */
      .leaflet-control-zoom {
        border: none !important;
        box-shadow: 0 4px 6px -1px rgb(0 0 0 / 0.1) !important;
      }
    "))
  ),
  
  # 9) Value Boxes on Top showing overall statistics and target percentages
  layout_column_wrap(
    width = 1/5,
    fill = FALSE,
    style = "margin-bottom: 8px; margin-top: 2px;",
    
    value_box(
      title = "TOTAL ENCUESTAS",
      value = textOutput("vb_total_val"),
      showcase = icon("clipboard-list"),
      class = "bg-gradient-total",
      uiOutput("vb_total_sub")
    ),
    value_box(
      title = "GRUPO CONTROL",
      value = textOutput("vb_control_val"),
      showcase = icon("scale-balanced"),
      class = "bg-gradient-control",
      uiOutput("vb_control_sub")
    ),
    value_box(
      title = "TRATAMIENTO 1",
      value = textOutput("vb_t1_val"),
      showcase = icon("flask-vial"),
      class = "bg-gradient-t1",
      uiOutput("vb_t1_sub")
    ),
    value_box(
      title = "TRATAMIENTO 2",
      value = textOutput("vb_t2_val"),
      showcase = icon("flask"),
      class = "bg-gradient-t2",
      uiOutput("vb_t2_sub")
    ),
    value_box(
      title = "CENTROS VISITADOS",
      value = textOutput("vb_schools_val"),
      showcase = icon("school"),
      class = "bg-gradient-schools",
      uiOutput("vb_schools_sub")
    )
  ),
  
  # 5) Main body tabset panel containing the map and control tables
  navset_card_tab(
    height = "820px",
    
    # Tab 1: Mapa de Coordenadas
    nav_panel(
      title = "Mapa de Coordenadas",
      icon = icon("map-location-dot"),
      leafletOutput("map", height = "100%")
    ),
    
    # Tab 2: Control por Observador (Aggregated)
    nav_panel(
      title = "Control por Observador",
      icon = icon("users-viewfinder"),
      div(
        style = "padding: 15px; height: 100%; display: flex; flex-direction: column;",
        h4("Progreso Acumulado por Código de Encuestador", style = "color: #1e3a8a; font-weight: 700; margin-bottom: 15px; font-size: 16px;"),
        div(style = "flex-grow: 1; overflow-y: auto;", DTOutput("table_control"))
      )
    ),
    
    # Tab 3: Detalle de Registros (Disaggregated)
    nav_panel(
      title = "Detalle de Registros",
      icon = icon("database"),
      div(
        style = "padding: 15px; height: 100%; display: flex; flex-direction: column;",
        h4("Desagregado Completo de Boletas Levantadas", style = "color: #1e3a8a; font-weight: 700; margin-bottom: 15px; font-size: 16px;"),
        div(style = "flex-grow: 1; overflow-y: auto;", DTOutput("table_detalle"))
      )
    )
  )
)

# ----------------- SERVER LOGIC -----------------
server <- function(input, output, session) {
  
  # Reactive values to hold loaded dataset and metadata
  current_data <- reactiveVal(NULL)
  last_update <- reactiveVal(Sys.time())
  headers_hash <- reactiveVal("")
  
  # Function to fetch data and update reactive values
  update_data <- function(show_notification = FALSE) {
    df <- fetch_clean_kobo()
    if (!is.null(df)) {
      current_data(df)
      last_update(Sys.time())
      if (show_notification) {
        showNotification("Datos descargados correctamente de la API de KoboToolbox.", type = "message", duration = 4)
      }
    } else {
      if (show_notification) {
        showNotification("Error al descargar los datos. Se muestran datos guardados localmente.", type = "warning", duration = 5)
      }
    }
  }
  
  # Initial load from local backup or Kobo API on startup
  observe({
    if (is.null(current_data())) {
      # Try loading from local file first to make startup immediate
      if (file.exists("local_backup.rds")) {
        message("[STARTUP] Cargando copia de seguridad local para inicio inmediato...")
        current_data(readRDS("local_backup.rds"))
        last_update(file.mtime("local_backup.rds"))
        
        # Then, trigger an update from API in the background
        session$onFlushed(function() {
          update_data(show_notification = FALSE)
        }, once = TRUE)
      } else {
        # If no local backup, perform initial fetch synchronously
        update_data(show_notification = FALSE)
      }
    }
  })
  
  # 10) Auto-poll: Checks Kobo API headers every 30 seconds. Downloads only if modified or content-length changed
  auto_poll_timer <- reactiveTimer(30000)
  observeEvent(auto_poll_timer(), {
    req(current_data())
    tryCatch({
      h <- curl::curlGetHeaders(kobo_url)
      lm <- grep("last-modified", h, ignore.case = TRUE, value = TRUE)
      cl <- grep("content-length", h, ignore.case = TRUE, value = TRUE)
      current_hash <- paste(lm, cl, collapse = "_")
      
      # If headers changed, reload from Kobo ToolBox
      if (headers_hash() != current_hash) {
        headers_hash(current_hash)
        update_data(show_notification = TRUE)
      }
    }, error = function(e) {
      # Silent error, do not interrupt execution if internet drops
    })
  })
  
  # Manual Force Refresh Button
  observeEvent(input$refresh_btn, {
    withProgress(message = 'Conectando con KoboToolbox...', value = 0, {
      setProgress(0.3, detail = "Verificando actualizaciones...")
      update_data(show_notification = TRUE)
      setProgress(1.0, detail = "Actualización finalizada")
    })
  })
  
  # Print the date text when updated
  output$last_update_text <- renderText({
    paste("Actualizado:", format(last_update(), "%d/%m/%Y %H:%M:%S"))
  })
  
  # 8) Populate and update responsive filters dynamically from loaded data
  observe({
    req(current_data())
    df <- current_data()
    
    # Extract unique choices
    deptos <- sort(unique(df$departamento))
    zonas <- sort(unique(df$zona))
    grupos <- sort(unique(df$grupos_t_oculto))
    observadores <- sort(unique(df$id_observador))
    
    # Update filters (maintain selections if already chosen)
    updateSelectizeInput(session, "filter_depto", choices = deptos, selected = input$filter_depto)
    updateSelectizeInput(session, "filter_zona", choices = zonas, selected = input$filter_zona)
    updateSelectizeInput(session, "filter_grupo", choices = grupos, selected = input$filter_grupo)
    updateSelectizeInput(session, "filter_obs", choices = observadores, selected = input$filter_obs)
  })
  
  # Update districts based on selected departments
  observeEvent(input$filter_depto, {
    req(current_data())
    df <- current_data()
    
    if (is.null(input$filter_depto) || length(input$filter_depto) == 0) {
      distritos <- sort(unique(df$distrito))
    } else {
      distritos <- sort(unique(df$distrito[df$departamento %in% input$filter_depto]))
    }
    
    updateSelectizeInput(session, "filter_distrito", choices = distritos, selected = input$filter_distrito)
  }, ignoreNULL = FALSE)
  
  # Initialize the date range filter once
  date_initialized <- reactiveVal(FALSE)
  observe({
    req(current_data())
    if (!date_initialized()) {
      df <- current_data()
      if (nrow(df) > 0 && "fecha_en_que_se_desarrollo_la_clase" %in% colnames(df)) {
        min_date <- min(df$fecha_en_que_se_desarrollo_la_clase, na.rm = TRUE)
        max_date <- max(df$fecha_en_que_se_desarrollo_la_clase, na.rm = TRUE)
        updateDateRangeInput(
          session, "filter_date",
          start = min_date,
          end = max_date,
          min = min_date,
          max = max_date
        )
        date_initialized(TRUE)
      }
    }
  })
  
  # Reset filters action
  observeEvent(input$clear_filters_btn, {
    updateSelectizeInput(session, "filter_depto", selected = character(0))
    updateSelectizeInput(session, "filter_distrito", selected = character(0))
    updateSelectizeInput(session, "filter_zona", selected = character(0))
    updateSelectizeInput(session, "filter_grupo", selected = character(0))
    updateSelectizeInput(session, "filter_obs", selected = character(0))
    
    req(current_data())
    df <- current_data()
    if (nrow(df) > 0) {
      min_date <- min(df$fecha_en_que_se_desarrollo_la_clase, na.rm = TRUE)
      max_date <- max(df$fecha_en_que_se_desarrollo_la_clase, na.rm = TRUE)
      updateDateRangeInput(session, "filter_date", start = min_date, end = max_date)
    }
  })
  
  # Responsive filtered data reactive
  filtered_data <- reactive({
    req(current_data())
    df <- current_data()
    
    if (nrow(df) == 0) return(df)
    
    # Apply Department filter
    if (!is.null(input$filter_depto) && length(input$filter_depto) > 0) {
      df <- df %>% filter(departamento %in% input$filter_depto)
    }
    
    # Apply District filter
    if (!is.null(input$filter_distrito) && length(input$filter_distrito) > 0) {
      df <- df %>% filter(distrito %in% input$filter_distrito)
    }
    
    # Apply Zona filter
    if (!is.null(input$filter_zona) && length(input$filter_zona) > 0) {
      df <- df %>% filter(zona %in% input$filter_zona)
    }
    
    # Apply Grupo T filter
    if (!is.null(input$filter_grupo) && length(input$filter_grupo) > 0) {
      df <- df %>% filter(grupos_t_oculto %in% input$filter_grupo)
    }
    
    # Apply ID Observador filter
    if (!is.null(input$filter_obs) && length(input$filter_obs) > 0) {
      df <- df %>% filter(id_observador %in% input$filter_obs)
    }
    
    # Apply Date Range filter
    if (!is.null(input$filter_date) && length(input$filter_date) == 2 && !any(is.na(input$filter_date))) {
      df <- df %>% filter(
        fecha_en_que_se_desarrollo_la_clase >= input$filter_date[1] &
        fecha_en_que_se_desarrollo_la_clase <= input$filter_date[2]
      )
    }
    
    return(df)
  })
  
  # Helper to parse group counts safely
  parsed_groups <- reactive({
    df <- filtered_data()
    grupos <- tolower(as.character(df$grupos_t_oculto))
    
    control_count <- sum(grepl("control", grupos), na.rm = TRUE)
    t1_count <- sum(grepl("tratamiento\\s*1|t1", grupos), na.rm = TRUE)
    t2_count <- sum(grepl("tratamiento\\s*2|t2", grupos), na.rm = TRUE)
    
    list(
      Total = nrow(df),
      Control = control_count,
      T1 = t1_count,
      T2 = t2_count,
      Schools = n_distinct(df$nombre_del_centro_educativo)
    )
  })
  
  # ----------------- VALUE BOXES RENDER -----------------
  
  # Total Value Box
  output$vb_total_val <- renderText({
    pg <- parsed_groups()
    paste0(format(pg$Total, big.mark = ","), " / ", format(TARGET_TOTAL, big.mark = ","))
  })
  output$vb_total_sub <- renderUI({
    pg <- parsed_groups()
    pct <- round((pg$Total / TARGET_TOTAL) * 100, 2)
    HTML(paste0("Progreso: <strong>", pct, "%</strong> del total"))
  })
  
  # Control Value Box
  output$vb_control_val <- renderText({
    pg <- parsed_groups()
    paste0(format(pg$Control, big.mark = ","), " / ", format(TARGET_CONTROL, big.mark = ","))
  })
  output$vb_control_sub <- renderUI({
    pg <- parsed_groups()
    pct <- round((pg$Control / TARGET_CONTROL) * 100, 2)
    HTML(paste0("Progreso: <strong>", pct, "%</strong> de la meta"))
  })
  
  # Tratamiento 1 Value Box
  output$vb_t1_val <- renderText({
    pg <- parsed_groups()
    paste0(format(pg$T1, big.mark = ","), " / ", format(TARGET_T1, big.mark = ","))
  })
  output$vb_t1_sub <- renderUI({
    pg <- parsed_groups()
    pct <- round((pg$T1 / TARGET_T1) * 100, 2)
    HTML(paste0("Progreso: <strong>", pct, "%</strong> de la meta"))
  })
  
  # Tratamiento 2 Value Box
  output$vb_t2_val <- renderText({
    pg <- parsed_groups()
    paste0(format(pg$T2, big.mark = ","), " / ", format(TARGET_T2, big.mark = ","))
  })
  output$vb_t2_sub <- renderUI({
    pg <- parsed_groups()
    pct <- round((pg$T2 / TARGET_T2) * 100, 2)
    HTML(paste0("Progreso: <strong>", pct, "%</strong> de la meta"))
  })
  
  # Schools Value Box
  output$vb_schools_val <- renderText({
    pg <- parsed_groups()
    format(pg$Schools, big.mark = ",")
  })
  output$vb_schools_sub <- renderUI({
    HTML("Centros escolares únicos visitados")
  })
  
  # ----------------- MAP RENDER -----------------
  
  # Initialize Map
  output$map <- renderLeaflet({
    leaflet() %>%
      # Add 3 provider tiles for Calle, Satelite, Oscuro
      addProviderTiles(providers$CartoDB.Positron, group = "Calles") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Oscuro") %>%
      setView(lng = -88.89653, lat = 13.794185, zoom = 8) %>% # Centered in El Salvador
      addLegend(
        position = "bottomright",
        colors = c("#1e3a8a", "#0f766e", "#9d174d", "#4b5563"),
        labels = c("Control", "Tratamiento 1", "Tratamiento 2", "Desconocido"),
        title = "Grupo de Tratamiento",
        opacity = 0.85
      ) %>%
      # Add layer control to toggle between map layers
      addLayersControl(
        baseGroups = c("Calles", "Satélite", "Oscuro"),
        options = layersControlOptions(collapsed = FALSE),
        position = "topright"
      )
  })
  
  # Reactive Map update based on filters
  observe({
    df <- filtered_data()
    
    # Filter valid coordinates
    map_df <- df %>%
      filter(!is.na(coordenadas_gps_del_centro_educativo_latitude) & 
             !is.na(coordenadas_gps_del_centro_educativo_longitude)) %>%
      filter(coordenadas_gps_del_centro_educativo_latitude != 0 & 
             coordenadas_gps_del_centro_educativo_longitude != 0)
    
    # Color helper for markers
    get_color <- function(group) {
      g <- tolower(as.character(group))
      ifelse(grepl("control", g), "#1e3a8a", # Deep blue
      ifelse(grepl("1", g), "#0f766e",       # Dark teal
      ifelse(grepl("2", g), "#9d174d",       # Dark pink
      "#4b5563")))                           # grey fallback
    }
    
    # We check if the photo url column exists and construct HTML safely
    url_col <- "fotografia_del_consentimiento_informado_llenado_y_firmado_por_el_docente_url"
    photo_html <- ""
    if (url_col %in% colnames(map_df)) {
      photo_html <- ifelse(
        is.na(map_df[[url_col]]) | map_df[[url_col]] == "",
        "",
        paste0(
          "<div style='margin-top: 10px; text-align: center;'>",
            "<a href='", map_df[[url_col]], "' target='_blank' style='text-decoration: none;'>",
              "<img src='", map_df[[url_col]], "' style='width: 100%; max-height: 120px; object-fit: cover; border-radius: 6px; border: 1px solid #cbd5e1;' onerror='this.onerror=null; this.parentElement.innerHTML=\"<span style=\\\"font-size: 11px; color: #2563eb; font-weight: 600; text-decoration: underline;\\\">Foto de evidencia (abrir enlace)</span>\";' />",
            "</a>",
          "</div>"
        )
      )
    } else {
      photo_html <- rep("", nrow(map_df))
    }
    
    # 4) Construct Popups with: index, fecha, nombre del educativo, zona, departamento, distrito, cluster, grupoT, id observador y fecha
    popups <- paste0(
      "<div style='font-family: system-ui, -apple-system, sans-serif; font-size: 13px; line-height: 1.6; min-width: 240px; padding: 5px;'>",
        "<h6 style='margin: 0 0 10px 0; color: #2563eb; border-bottom: 2px solid #e2e8f0; padding-bottom: 6px; font-weight: 700; font-size: 14px;'>Boleta N° ", map_df$index, "</h6>",
        "<table style='width: 100%; border-collapse: collapse;'>",
          "<tr><td style='padding: 3px 0; color: #64748b; font-weight: 600; width: 40%; vertical-align: top;'>C. Educativo:</td><td style='padding: 3px 0; font-weight: 600; color: #1e293b;'>", map_df$nombre_del_centro_educativo, "</td></tr>",
          "<tr><td style='padding: 3px 0; color: #64748b; font-weight: 600; vertical-align: top;'>Observador:</td><td style='padding: 3px 0; color: #0f172a; font-weight: 500;'>ID ", map_df$id_observador, "</td></tr>",
          "<tr><td style='padding: 3px 0; color: #64748b; font-weight: 600; vertical-align: top;'>Grupo T:</td><td style='padding: 3px 0;'><span style='background-color: ", 
            ifelse(grepl("control", tolower(map_df$grupos_t_oculto)), "#dbeafe; color: #1e40af;", 
            ifelse(grepl("1", map_df$grupos_t_oculto), "#ccfbf1; color: #0f766e;", "#fce7f3; color: #9d174d;")), 
            "; padding: 2px 8px; border-radius: 4px; font-weight: 700; font-size: 11px;'>", map_df$grupos_t_oculto, "</span></td></tr>",
          "<tr><td style='padding: 3px 0; color: #64748b; font-weight: 600; vertical-align: top;'>Depto:</td><td style='padding: 3px 0; color: #334155;'>", map_df$departamento, "</td></tr>",
          "<tr><td style='padding: 3px 0; color: #64748b; font-weight: 600; vertical-align: top;'>Distrito:</td><td style='padding: 3px 0; color: #334155;'>", map_df$distrito, "</td></tr>",
          "<tr><td style='padding: 3px 0; color: #64748b; font-weight: 600; vertical-align: top;'>Zona:</td><td style='padding: 3px 0; color: #334155;'>", map_df$zona, "</td></tr>",
          "<tr><td style='padding: 3px 0; color: #64748b; font-weight: 600; vertical-align: top;'>Cluster:</td><td style='padding: 3px 0; color: #334155;'>", map_df$cluster_oculto, "</td></tr>",
          "<tr><td style='padding: 3px 0; color: #64748b; font-weight: 600; vertical-align: top;'>Fecha Clase:</td><td style='padding: 3px 0; color: #334155; font-weight: 500;'>", format(map_df$fecha_en_que_se_desarrollo_la_clase, "%d/%m/%Y"), "</td></tr>",
          "<tr><td style='padding: 3px 0; color: #64748b; font-weight: 600; vertical-align: top;'>Fecha Envío:</td><td style='padding: 3px 0; color: #334155;'>", format(map_df$submission_time, "%d/%m/%Y %H:%M"), "</td></tr>",
        "</table>",
        photo_html,
      "</div>"
    )
    
    leafletProxy("map") %>%
      clearMarkers()
    
    if (nrow(map_df) > 0) {
      leafletProxy("map", data = map_df) %>%
        addCircleMarkers(
          lng = ~coordenadas_gps_del_centro_educativo_longitude,
          lat = ~coordenadas_gps_del_centro_educativo_latitude,
          radius = 8,
          color = "#ffffff",
          weight = 1.5,
          fillColor = ~get_color(grupos_t_oculto),
          fillOpacity = 0.85,
          popup = popups,
          label = ~paste0("Boleta #", index, " — ", nombre_del_centro_educativo),
          layerId = ~index
        )
      
      # Zoom to show all markers dynamically if there are any
      bounds <- map_df %>%
        summarise(
          min_lat = min(coordenadas_gps_del_centro_educativo_latitude),
          max_lat = max(coordenadas_gps_del_centro_educativo_latitude),
          min_lng = min(coordenadas_gps_del_centro_educativo_longitude),
          max_lng = max(coordenadas_gps_del_centro_educativo_longitude)
        )
      
      if (nrow(map_df) > 1) {
        leafletProxy("map") %>%
          fitBounds(bounds$min_lng, bounds$min_lat, bounds$max_lng, bounds$max_lat)
      } else {
        leafletProxy("map") %>%
          setView(lng = map_df$coordenadas_gps_del_centro_educativo_longitude[1], 
                  lat = map_df$coordenadas_gps_del_centro_educativo_latitude[1], 
                  zoom = 12)
      }
    }
  })
  
  # ----------------- TABLES RENDER -----------------
  
  # 5) Tab 2 Table: Control por Observador (Aggregated)
  # Aggregated by observer ID: survey counts by department, district, Group T, and sex of teachers
  output$table_control <- renderDT({
    req(filtered_data())
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(datatable(tibble(Mensaje = "No hay datos que coincidan con los filtros seleccionados.")))
    }
    
    control_obs <- df %>%
      group_by(id_observador, departamento, distrito, grupos_t_oculto) %>%
      summarise(
        total_encuestas = n(),
        docentes_femenino = sum(sexo_del_docente == "Femenino", na.rm = TRUE),
        docentes_masculino = sum(sexo_del_docente == "Masculino", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rename(
        `Código Encuestador (ID)` = id_observador,
        `Departamento` = departamento,
        `Distrito` = distrito,
        `Grupo T` = grupos_t_oculto,
        `Total Encuestas` = total_encuestas,
        `Docentes Fem.` = docentes_femenino,
        `Docentes Masc.` = docentes_masculino
      )
    
    # 7) Enable download in Excel and PDF formats
    datatable(
      control_obs,
      extensions = c("Buttons", "Responsive"),
      rownames = FALSE,
      class = "cell-border stripe hover",
      options = list(
        dom = 'Bfrtip',
        buttons = list(
          list(
            extend = 'excel',
            text = '<i class="fa fa-file-excel" style="color: #16a34a; margin-right: 5px;"></i> Excel',
            filename = paste0("Control_Observador_", format(Sys.time(), "%Y%m%d_%H%M%S")),
            title = "Informe de Control de Levantamiento por Observador (EVA Teach)"
          ),
          list(
            extend = 'pdf',
            text = '<i class="fa fa-file-pdf" style="color: #dc2626; margin-right: 5px;"></i> PDF',
            filename = paste0("Control_Observador_", format(Sys.time(), "%Y%m%d_%H%M%S")),
            title = "Informe de Control de Levantamiento por Observador (EVA Teach)",
            orientation = 'landscape',
            pageSize = 'A4'
          ),
          list(
            extend = 'copy',
            text = 'Copiar'
          )
        ),
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Spanish.json'
        ),
        pageLength = 50,
        lengthMenu = c(10, 25, 50, 100),
        columnDefs = list(
          list(className = 'dt-center', targets = c(0, 3, 4, 5, 6))
        )
      )
    )
  })
  
  # 6) Tab 3 Table: Detalle de Registros (Disaggregated)
  # Individual records with observer ID, school name, department, district, Group T, and survey date
  output$table_detalle <- renderDT({
    req(filtered_data())
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(datatable(tibble(Mensaje = "No hay datos que coincidan con los filtros seleccionados.")))
    }
    
    detalle <- df %>%
      select(
        index,
        id_observador,
        nombre_del_centro_educativo,
        departamento,
        distrito,
        grupos_t_oculto,
        fecha_en_que_se_desarrollo_la_clase
      ) %>%
      mutate(
        fecha_en_que_se_desarrollo_la_clase = format(fecha_en_que_se_desarrollo_la_clase, "%d/%m/%Y")
      ) %>%
      rename(
        `N° Boleta` = index,
        `Código Encuestador (ID)` = id_observador,
        `Centro Educativo` = nombre_del_centro_educativo,
        `Departamento` = departamento,
        `Distrito` = distrito,
        `Grupo T` = grupos_t_oculto,
        `Fecha de Registro` = fecha_en_que_se_desarrollo_la_clase
      )
    
    # 7) Enable download in Excel and PDF formats
    datatable(
      detalle,
      extensions = c("Buttons", "Responsive"),
      rownames = FALSE,
      class = "cell-border stripe hover",
      options = list(
        dom = 'Bfrtip',
        buttons = list(
          list(
            extend = 'excel',
            text = '<i class="fa fa-file-excel" style="color: #16a34a; margin-right: 5px;"></i> Excel',
            filename = paste0("Detalle_Registros_", format(Sys.time(), "%Y%m%d_%H%M%S")),
            title = "Informe de Detalle de Registros - Levantamiento (EVA Teach)"
          ),
          list(
            extend = 'pdf',
            text = '<i class="fa fa-file-pdf" style="color: #dc2626; margin-right: 5px;"></i> PDF',
            filename = paste0("Detalle_Registros_", format(Sys.time(), "%Y%m%d_%H%M%S")),
            title = "Informe de Detalle de Registros - Levantamiento (EVA Teach)",
            orientation = 'landscape',
            pageSize = 'A4'
          ),
          list(
            extend = 'copy',
            text = 'Copiar'
          )
        ),
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Spanish.json'
        ),
        pageLength = 50,
        lengthMenu = c(10, 25, 50, 100),
        columnDefs = list(
          list(className = 'dt-center', targets = c(0, 1, 5, 6))
        )
      )
    )
  })
  
}

# Run the Shiny application
shinyApp(ui = ui, server = server)
