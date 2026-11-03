# ==============================================================================
# DATA DRIVEN CRIME ANALYTICS SYSTEM USING R
# Premium Glassmorphism Redesign
# ==============================================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(leaflet)

# Source internal modules
source("R/data_collection.R")
source("R/preprocessing.R")
source("R/crime_analysis.R")
source("R/visualization.R")
source("R/report_generation.R")

# Shiny UI Definition
ui <- dashboardPage(
  skin = "black",
  
  # Hide AdminLTE default top navbar
  dashboardHeader(disable = TRUE),
  
  # Sidebar Navigation Matching Reference Image
  dashboardSidebar(
    width = 260,
    tags$div(
      class = "brand-header",
      tags$div(
        class = "brand-icon-wrapper",
        icon("shield-alt")
      ),
      tags$div(
        class = "brand-title-group",
        tags$h3("Crime Analytics"),
        tags$p("Data Driven Insights")
      )
    ),
    
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "home", icon = icon("th-large")),
      menuItem("Data Collection", tabName = "collection", icon = icon("database")),
      menuItem("Data Preprocessing", tabName = "preprocessing", icon = icon("sliders-h")),
      menuItem("Crime Analysis & Visuals", tabName = "analytics", icon = icon("chart-bar")),
      menuItem("Report Generation", tabName = "report", icon = icon("file-export"))
    ),
    
    # Bottom Glass Promo Card (matching reference image)
    tags$div(
      class = "sidebar-footer-card",
      tags$div(
        class = "sfc-icon-wrapper",
        icon("chart-line")
      ),
      tags$h5("Safer Communities Through Data"),
      tags$p("Turning crime data into meaningful insights for a safer tomorrow.")
    )
  ),
  
  # Dashboard Main Body
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$title("Crime Analytics System | Data Driven Insights")
    ),
    
    # Ambient Animated Background Layer (Fixed behind content)
    tags$div(
      class = "ambient-bg-layer",
      tags$div(class = "ambient-orb ambient-orb-1"),
      tags$div(class = "ambient-orb ambient-orb-2"),
      tags$div(class = "ambient-orb ambient-orb-3"),
      tags$div(class = "ambient-topography-lines"),
      tags$div(class = "ambient-particle particle-1"),
      tags$div(class = "ambient-particle particle-2"),
      tags$div(class = "ambient-particle particle-3"),
      tags$div(class = "ambient-particle particle-4")
    ),
    
    # Top Navigation Bar (Search, Notifications, Profile)
    tags$div(
      class = "top-nav-bar",
      tags$div(
        class = "search-container",
        icon("search"),
        tags$input(
          type = "text",
          class = "search-input",
          placeholder = "Search crime type, area, or location...",
          id = "global_search"
        )
      ),
      tags$div(
        class = "top-actions-group",
        tags$div(
          class = "action-icon-btn",
          title = "System Alerts",
          icon("bell"),
          tags$span(class = "notification-badge-dot")
        ),
        tags$div(
          class = "profile-pill",
          tags$div(class = "profile-avatar", "CA"),
          tags$span(class = "profile-name", "Crime Analyst"),
          icon("chevron-down", class = "profile-caret")
        )
      )
    ),
    
    # Sub-Header Greeting & Quick Time Filter
    tags$div(
      class = "greeting-section",
      tags$div(
        class = "greeting-text-group",
        tags$div(class = "greeting-sup", "Good Morning,"),
        tags$h1("Here's Your Crime Overview"),
        tags$p("Explore crime patterns, trends and insights from your dataset.")
      ),
      tags$div(
        class = "date-filter-pill",
        icon("calendar-alt"),
        tags$span("Last 7 Days"),
        icon("chevron-down", style = "font-size:10px; color:#8A9995;")
      )
    ),
    
    tabItems(
      # ========================================================================
      # TAB 1: DASHBOARD HOME (Matching Reference Image)
      # ========================================================================
      tabItem(
        tabName = "home",
        
        # ROW 1: 4 High-Impact KPI Stat Cards
        tags$div(
          class = "kpi-grid",
          
          # Card 1: Total Records
          tags$div(
            class = "kpi-stat-card",
            tags$div(
              class = "kpi-top-row",
              tags$div(
                tags$div(class = "kpi-label", "Total Crime Records"),
                tags$div(class = "kpi-value-text", textOutput("kpi_home_total", inline = TRUE))
              ),
              tags$div(class = "kpi-icon-circle kpi-icon-green", icon("database"))
            ),
            tags$div(
              class = "kpi-bottom-row",
              tags$div(
                tags$span(class = "kpi-trend-pill", icon("arrow-up"), "12%"),
                tags$span(class = "kpi-trend-subtext", "vs. previous period")
              ),
              tags$svg(
                class = "kpi-sparkline-svg", viewBox = "0 0 65 24", fill = "none",
                tags$path(d = "M2 18 C 15 17, 25 22, 38 12 C 48 4, 55 10, 63 3", stroke = "#2D9B72", `stroke-width` = "2.2", `stroke-linecap` = "round")
              )
            )
          ),
          
          # Card 2: Distinct Crime Types
          tags$div(
            class = "kpi-stat-card",
            tags$div(
              class = "kpi-top-row",
              tags$div(
                tags$div(class = "kpi-label", "Distinct Crime Types"),
                tags$div(class = "kpi-value-text", textOutput("kpi_home_types", inline = TRUE))
              ),
              tags$div(class = "kpi-icon-circle kpi-icon-purple", icon("mask"))
            ),
            tags$div(
              class = "kpi-bottom-row",
              tags$div(
                tags$span(class = "kpi-trend-pill trend-blue", icon("arrow-up"), "8%"),
                tags$span(class = "kpi-trend-subtext", "vs. previous period")
              ),
              tags$svg(
                class = "kpi-sparkline-svg", viewBox = "0 0 65 24", fill = "none",
                tags$path(d = "M2 16 C 18 19, 30 14, 42 16 C 50 17, 56 10, 63 4", stroke = "#6C8EEF", `stroke-width` = "2.2", `stroke-linecap` = "round")
              )
            )
          ),
          
          # Card 3: Coverage Areas
          tags$div(
            class = "kpi-stat-card",
            tags$div(
              class = "kpi-top-row",
              tags$div(
                tags$div(class = "kpi-label", "Coverage Areas"),
                tags$div(class = "kpi-value-text", textOutput("kpi_home_areas", inline = TRUE))
              ),
              tags$div(class = "kpi-icon-circle kpi-icon-teal", icon("map-marker-alt"))
            ),
            tags$div(
              class = "kpi-bottom-row",
              tags$div(
                tags$span(class = "kpi-trend-pill", icon("arrow-up"), "0%"),
                tags$span(class = "kpi-trend-subtext", "vs. previous period")
              ),
              tags$svg(
                class = "kpi-sparkline-svg", viewBox = "0 0 65 24", fill = "none",
                tags$path(d = "M2 12 C 15 8, 25 18, 38 10 C 48 4, 55 12, 63 6", stroke = "#2D9B72", `stroke-width` = "2.2", `stroke-linecap` = "round")
              )
            )
          ),
          
          # Card 4: System Status
          tags$div(
            class = "kpi-stat-card",
            tags$div(
              class = "kpi-top-row",
              tags$div(
                tags$div(class = "kpi-label", "System Status"),
                tags$div(class = "kpi-value-text", style = "font-size: 23px;", textOutput("kpi_home_status", inline = TRUE))
              ),
              tags$div(class = "kpi-icon-circle kpi-icon-amber", icon("cogs"))
            ),
            tags$div(
              class = "kpi-bottom-row",
              tags$div(
                tags$span(class = "kpi-trend-subtext", style = "margin-left:0;", "Dataset ready for analysis")
              ),
              tags$div(
                style = "color: #2D9B72; font-size: 16px;",
                icon("check-circle")
              )
            )
          )
        ),
        
        # ROW 2: Middle Analytics Grid (Trends Spline, Donut, Top 5 Areas)
        tags$div(
          class = "analytics-middle-grid",
          
          # 1. Crime Trends Over Time
          tags$div(
            class = "glass-card",
            tags$div(
              class = "glass-card-header",
              tags$div(
                class = "gch-left",
                tags$div(class = "gch-icon", icon("chart-line")),
                tags$div(
                  class = "gch-title-group",
                  tags$h4("Crime Trends Over Time"),
                  tags$p("Monthly crime count and pattern")
                )
              ),
              tags$div(
                class = "date-filter-pill",
                style = "padding: 4px 10px; font-size: 11px;",
                tags$span("Monthly"),
                icon("chevron-down", style = "font-size:9px; color:#8A9995;")
              )
            ),
            plotlyOutput("home_plot_trends", height = "240px")
          ),
          
          # 2. Crime by Type Donut
          tags$div(
            class = "glass-card",
            tags$div(
              class = "glass-card-header",
              tags$div(
                class = "gch-left",
                tags$div(class = "gch-icon", icon("lightbulb")),
                tags$div(
                  class = "gch-title-group",
                  tags$h4("Crime by Type"),
                  tags$p("Distribution of total crimes")
                )
              )
            ),
            plotlyOutput("home_plot_type_donut", height = "240px")
          ),
          
          # 3. Top 5 Areas
          tags$div(
            class = "glass-card",
            tags$div(
              class = "glass-card-header",
              tags$div(
                class = "gch-left",
                tags$div(class = "gch-icon", icon("map-pin")),
                tags$div(
                  class = "gch-title-group",
                  tags$h4("Top 5 Areas"),
                  tags$p("Areas with highest crime count")
                )
              )
            ),
            uiOutput("home_top_areas_ui")
          )
        ),
        
        # ROW 3: Bottom Analytics Grid (Heatmap & Insights, Recent Records Table)
        tags$div(
          class = "analytics-bottom-grid",
          
          # Heatmap & Quick Insights Card
          tags$div(
            class = "glass-card",
            tags$div(
              class = "glass-card-header",
              tags$div(
                class = "gch-left",
                tags$div(class = "gch-icon", icon("fire")),
                tags$div(
                  class = "gch-title-group",
                  tags$h4("Crime Heatmap"),
                  tags$p("Area by crime category")
                )
              )
            ),
            tags$div(
              class = "heatmap-split-container",
              tags$div(
                class = "heatmap-canvas-wrapper",
                plotlyOutput("home_heatmap_plot", height = "210px")
              ),
              tags$div(
                class = "quick-insights-box",
                tags$h5("Quick Insights"),
                uiOutput("home_quick_insights_ui")
              )
            )
          ),
          
          # Recent Records Table Card
          tags$div(
            class = "glass-card",
            tags$div(
              class = "glass-card-header",
              tags$div(
                class = "gch-left",
                tags$div(class = "gch-icon", icon("file-alt")),
                tags$div(
                  class = "gch-title-group",
                  tags$h4("Recent Records"),
                  tags$p("Latest reported crime incidents")
                )
              ),
              actionLink("btn_goto_records", "View All →", style = "font-size:12.5px; font-weight:700; color:#2D9B72; text-decoration:none;")
            ),
            uiOutput("home_recent_records_ui")
          )
        ),
        
        # Academic Project Specifications (Preserved in an elegant glass card)
        tags$div(
          class = "glass-card",
          style = "background: rgba(255, 255, 255, 0.65);",
          tags$div(
            class = "glass-card-header",
            tags$div(
              class = "gch-left",
              tags$div(class = "gch-icon", icon("graduation-cap")),
              tags$div(
                class = "gch-title-group",
                tags$h4("Academic Specifications & System Objectives"),
                tags$p("A Data Driven Crime Analytics System using R")
              )
            )
          ),
          fluidRow(
            column(
              width = 7,
              tags$p(strong("Problem Statement: "), "Urban police agencies face overwhelming volumes of heterogeneous crime data. This automated data-driven analytical pipeline discovers spatio-temporal crime patterns, victim vulnerabilities, and clearance efficiency."),
              tags$p(strong("Domain & Core Stack: "), "Police Intelligence & Big Data Analytics | Built with R, Shiny, ggplot2, Plotly, Leaflet, DT, dplyr, rmarkdown.")
            ),
            column(
              width = 5,
              tags$p(strong("Architecture Pipeline:")),
              tags$p("1. Data Collection → 2. Preprocessing → 3. Statistical Analysis → 4. Visualizations & Leaflet → 5. Automated Reports.")
            )
          )
        )
      ),
      
      # ========================================================================
      # TAB 2: DATA COLLECTION
      # ========================================================================
      tabItem(
        tabName = "collection",
        fluidRow(
          column(
            width = 4,
            tags$div(
              class = "glass-card",
              tags$h4(style = "font-weight:700; margin-bottom:16px; color:#172A2A;", icon("database"), " Data Source Control"),
              fileInput("file_upload", "Upload Crime CSV Dataset:", accept = c(".csv"), width = "100%"),
              tags$div(style = "text-align: center; margin: 15px 0; color:#8A9995; font-size:12px; font-weight:700;", "— OR —"),
              actionButton("btn_sample_data", "Load Built-in Sample Dataset", icon = icon("file-csv"), class = "btn-primary", style = "width: 100%;"),
              hr(style = "border-color: rgba(220, 235, 217, 0.6); margin: 20px 0;"),
              uiOutput("validation_status_ui")
            )
          ),
          column(
            width = 8,
            tags$div(
              class = "glass-card",
              tags$h4(style = "font-weight:700; margin-bottom:16px; color:#172A2A;", icon("info-circle"), " Raw Dataset Metadata & Metrics"),
              tags$div(
                class = "kpi-grid",
                style = "margin-bottom: 16px;",
                tags$div(class = "kpi-stat-card", tags$div(class = "kpi-label", "Total Records"), tags$div(class = "kpi-value-text", textOutput("col_meta_records_txt"))),
                tags$div(class = "kpi-stat-card", tags$div(class = "kpi-label", "Total Columns"), tags$div(class = "kpi-value-text", textOutput("col_meta_cols_txt"))),
                tags$div(class = "kpi-stat-card", tags$div(class = "kpi-label", "Memory Size"), tags$div(class = "kpi-value-text", style = "font-size:22px;", textOutput("col_meta_size_txt"))),
                tags$div(class = "kpi-stat-card", tags$div(class = "kpi-label", "Missing Cells"), tags$div(class = "kpi-value-text", textOutput("col_meta_missing_txt")))
              ),
              tags$h5(strong("Dataset Column Schema:"), style = "margin-top:14px; font-size:13px; color:#647875;"),
              verbatimTextOutput("raw_column_names")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            tags$div(
              class = "glass-card",
              tags$h4(style = "font-weight:700; margin-bottom:16px; color:#172A2A;", icon("table"), " Raw Uploaded Data Preview"),
              DTOutput("raw_data_table")
            )
          )
        )
      ),
      
      # ========================================================================
      # TAB 3: DATA PREPROCESSING
      # ========================================================================
      tabItem(
        tabName = "preprocessing",
        fluidRow(
          column(
            width = 4,
            tags$div(
              class = "glass-card",
              tags$h4(style = "font-weight:700; margin-bottom:16px; color:#172A2A;", icon("sliders-h"), " Preprocessing Controls"),
              tags$h5(strong("Data Quality Audit (Raw State):"), style = "font-size:13px; color:#647875;"),
              uiOutput("before_prep_stats_ui"),
              hr(style = "border-color: rgba(220, 235, 217, 0.6); margin: 20px 0;"),
              actionButton("btn_preprocess", "Run Preprocessing Pipeline", icon = icon("magic"), class = "btn-success", style = "width: 100%; margin-bottom: 15px;"),
              tags$h5(strong("Execution Console Log:"), style = "font-size:13px; color:#647875;"),
              verbatimTextOutput("preprocessing_log_output")
            )
          ),
          column(
            width = 8,
            tags$div(
              class = "glass-card",
              tags$h4(style = "font-weight:700; margin-bottom:16px; color:#172A2A;", icon("check-circle"), " Cleaned Dataset Verification"),
              tags$div(
                class = "kpi-grid",
                style = "grid-template-columns: repeat(3, 1fr); margin-bottom: 16px;",
                tags$div(class = "kpi-stat-card", tags$div(class = "kpi-label", "Cleaned Records"), tags$div(class = "kpi-value-text", textOutput("prep_meta_records_txt"))),
                tags$div(class = "kpi-stat-card", tags$div(class = "kpi-label", "Remaining NAs"), tags$div(class = "kpi-value-text", textOutput("prep_meta_missing_txt"))),
                tags$div(class = "kpi-stat-card", tags$div(class = "kpi-label", "Duplicates Eliminated"), tags$div(class = "kpi-value-text", textOutput("prep_meta_duplicates_txt")))
              ),
              tags$h5(strong("Transformation Pipeline Details:"), style = "font-size:13px; color:#647875; margin-top:14px;"),
              tags$ul(
                style = "font-size: 13px; color: #647875; line-height: 1.8;",
                tags$li(strong("Date Standardization: "), "Parsed into ISO standard YYYY-MM-DD."),
                tags$li(strong("Feature Engineering: "), "Derived Year, Month, Day, Day_of_Week, and Hour fields."),
                tags$li(strong("Category Inference: "), "Mapped individual Crime_Types to operational categories."),
                tags$li(strong("Median Imputation: "), "Filled missing victim ages with median value, standardized missing strings.")
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            tags$div(
              class = "glass-card",
              tags$h4(style = "font-weight:700; margin-bottom:16px; color:#172A2A;", icon("table"), " Preprocessed Cleaned Dataset"),
              DTOutput("clean_data_table")
            )
          )
        )
      ),
      
      # ========================================================================
      # TAB 4: CRIME ANALYSIS & VISUALS
      # ========================================================================
      tabItem(
        tabName = "analytics",
        
        # Interactive Dynamic Filter Control Bar
        tags$div(
          class = "glass-card",
          style = "padding: 16px 20px; margin-bottom: 20px;",
          fluidRow(
            column(2, selectInput("filter_year", "Select Year:", choices = c("All"), selected = "All")),
            column(2, selectInput("filter_city", "Select City:", choices = c("All"), selected = "All")),
            column(2, selectInput("filter_area", "Select Area:", choices = c("All"), selected = "All")),
            column(2, selectInput("filter_type", "Crime Type:", choices = c("All"), selected = "All")),
            column(2, selectInput("filter_category", "Crime Category:", choices = c("All"), selected = "All")),
            column(2, selectInput("filter_status", "Status:", choices = c("All"), selected = "All"))
          ),
          fluidRow(
            column(12, align = "right",
                   actionButton("btn_reset_filters", "Reset All Filters", icon = icon("redo"), class = "btn-default")
            )
          )
        ),
        
        # Analytic Sub-Tabs
        tabsetPanel(
          type = "tabs",
          
          # Tabset 1: Temporal Trends
          tabPanel(
            "Temporal Crime Trends", icon = icon("chart-line"),
            br(),
            fluidRow(
              column(6, tags$div(class = "glass-card", tags$h4(strong("Annual Incident Trend")), plotlyOutput("plot_year_trend"))),
              column(6, tags$div(class = "glass-card", tags$h4(strong("Monthly Crime Distribution")), plotlyOutput("plot_month_dist")))
            ),
            fluidRow(
              column(6, tags$div(class = "glass-card", tags$h4(strong("Weekly Crime Pattern")), plotlyOutput("plot_dow_dist"))),
              column(6, tags$div(class = "glass-card", tags$h4(strong("24-Hour Diurnal Pattern")), plotlyOutput("plot_hour_trend")))
            )
          ),
          
          # Tabset 2: Types & Hotspots
          tabPanel(
            "Crime Types & Hotspots", icon = icon("map-marker-alt"),
            br(),
            fluidRow(
              column(6, tags$div(class = "glass-card", tags$h4(strong("Specific Crime Types")), plotlyOutput("plot_type_dist"))),
              column(6, tags$div(class = "glass-card", tags$h4(strong("Crime Category Share")), plotlyOutput("plot_cat_donut")))
            ),
            fluidRow(
              column(6, tags$div(class = "glass-card", tags$h4(strong("Top Crime Areas")), plotlyOutput("plot_area_dist"))),
              column(6, tags$div(class = "glass-card", tags$h4(strong("Area vs Category Matrix")), plotlyOutput("plot_heatmap")))
            )
          ),
          
          # Tabset 3: Victim & Weapons
          tabPanel(
            "Victim & Weapon Demographics", icon = icon("users"),
            br(),
            fluidRow(
              column(6, tags$div(class = "glass-card", tags$h4(strong("Victim Gender Share")), plotlyOutput("plot_gender_donut"))),
              column(6, tags$div(class = "glass-card", tags$h4(strong("Victim Age Distribution")), plotlyOutput("plot_age_hist")))
            ),
            fluidRow(
              column(12, tags$div(class = "glass-card", tags$h4(strong("Weapon Involvement Frequency")), plotlyOutput("plot_weapon_bar")))
            )
          ),
          
          # Tabset 4: Status & Geographic Map
          tabPanel(
            "Case Status & Geographic Map", icon = icon("map"),
            br(),
            fluidRow(
              column(5, tags$div(class = "glass-card", tags$h4(strong("Case Resolution Status")), plotlyOutput("plot_status_donut"))),
              column(7, tags$div(class = "glass-card", tags$h4(strong("Geographic Interactive Map (Leaflet)")), leafletOutput("plot_leaflet_map", height = 400)))
            )
          )
        )
      ),
      
      # ========================================================================
      # TAB 5: REPORT GENERATION
      # ========================================================================
      tabItem(
        tabName = "report",
        fluidRow(
          column(
            width = 4,
            tags$div(
              class = "glass-card",
              tags$h4(style = "font-weight:700; margin-bottom:16px; color:#172A2A;", icon("file-export"), " Export Options"),
              selectInput("report_format", "Select Report Format:", choices = c("HTML Document (.html)" = "html", "PDF Document (.pdf)" = "pdf")),
              textInput("report_filename", "Output Report Filename:", value = "crime_analytics_report.html"),
              hr(style = "border-color: rgba(220, 235, 217, 0.6); margin: 20px 0;"),
              actionButton("btn_gen_report", "Generate Crime Analytics Report", icon = icon("file-download"), class = "btn-success", style = "width: 100%; margin-bottom: 15px;"),
              downloadButton("btn_download_report", "Download Generated Report", class = "btn-primary", style = "width: 100%;")
            )
          ),
          column(
            width = 8,
            tags$div(
              class = "glass-card",
              tags$h4(style = "font-weight:700; margin-bottom:16px; color:#172A2A;", icon("eye"), " Report Preview & Status"),
              uiOutput("report_status_msg"),
              hr(style = "border-color: rgba(220, 235, 217, 0.6); margin: 20px 0;"),
              htmlOutput("report_embedded_preview")
            )
          )
        )
      )
    ),
    
    # Dashboard Footer (matching reference image)
    tags$div(
      class = "dashboard-footer",
      tags$div("© 2025 Crime Analytics System | Built with R & Shiny"),
      tags$div(
        class = "footer-system-status",
        tags$span(class = "status-dot-pulse"),
        tags$span("System Online")
      )
    )
  )
)

# Shiny Server Definition
server <- function(input, output, session) {
  
  # Reactive State Store
  rv <- reactiveValues(
    raw_data = NULL,
    clean_data = NULL,
    is_preprocessed = FALSE,
    validation = list(valid = FALSE, message = "No dataset loaded yet."),
    before_stats = NULL,
    after_stats = NULL,
    cleaning_log = c(),
    generated_report_path = NULL
  )
  
  # ----------------------------------------------------------------------------
  # INITIALIZATION: Automatically Load Built-in Sample Dataset on Startup
  # ----------------------------------------------------------------------------
  observe({
    sample_df <- load_sample_data()
    rv$raw_data <- sample_df
    rv$validation <- validate_crime_data(sample_df)
    
    # Auto preprocess sample dataset for seamless initial display
    prep_res <- preprocess_data(sample_df)
    rv$clean_data <- prep_res$cleaned_data
    rv$before_stats <- prep_res$before_stats
    rv$after_stats <- prep_res$after_stats
    rv$cleaning_log <- prep_res$cleaning_log
    rv$is_preprocessed <- TRUE
  })
  
  # ----------------------------------------------------------------------------
  # NAVIGATION SHORTCUTS
  # ----------------------------------------------------------------------------
  observeEvent(input$btn_goto_records, {
    updateTabItems(session, "tabs", "analytics")
  })
  
  # ----------------------------------------------------------------------------
  # DASHBOARD HOME KPI OUTPUTS (Matching Reference Image)
  # ----------------------------------------------------------------------------
  output$kpi_home_total <- renderText({
    if (!is.null(rv$clean_data)) as.character(nrow(rv$clean_data)) else "0"
  })
  
  output$kpi_home_types <- renderText({
    if (!is.null(rv$clean_data) && "Crime_Type" %in% names(rv$clean_data)) {
      as.character(length(unique(rv$clean_data$Crime_Type)))
    } else "0"
  })
  
  output$kpi_home_areas <- renderText({
    if (!is.null(rv$clean_data) && "Area" %in% names(rv$clean_data)) {
      as.character(length(unique(rv$clean_data$Area)))
    } else "0"
  })
  
  output$kpi_home_status <- renderText({
    if (rv$is_preprocessed) "Preprocessed" else "Raw Dataset"
  })
  
  # Home Charts
  output$home_plot_trends <- renderPlotly({
    req(rv$clean_data)
    plot_home_trends(rv$clean_data)
  })
  
  output$home_plot_type_donut <- renderPlotly({
    req(rv$clean_data)
    plot_home_type_donut(rv$clean_data)
  })
  
  output$home_top_areas_ui <- renderUI({
    req(rv$clean_data)
    render_home_top_areas_html(rv$clean_data)
  })
  
  output$home_heatmap_plot <- renderPlotly({
    req(rv$clean_data)
    plot_home_heatmap(rv$clean_data)
  })
  
  output$home_quick_insights_ui <- renderUI({
    req(rv$clean_data)
    generate_home_quick_insights(rv$clean_data)
  })
  
  output$home_recent_records_ui <- renderUI({
    req(rv$clean_data)
    render_home_recent_records_html(rv$clean_data, limit = 5)
  })
  
  # Backward-compatible Home ValueBoxes for shinydashboard API
  output$home_kpi_total <- renderValueBox({
    cnt <- if(!is.null(rv$clean_data)) nrow(rv$clean_data) else 0
    valueBox(cnt, "Total Crime Records", icon = icon("shield-alt"), color = "green")
  })
  output$home_kpi_types <- renderValueBox({
    cnt <- if(!is.null(rv$clean_data) && "Crime_Type" %in% names(rv$clean_data)) length(unique(rv$clean_data$Crime_Type)) else 0
    valueBox(cnt, "Distinct Crime Types", icon = icon("list-alt"), color = "purple")
  })
  output$home_kpi_areas <- renderValueBox({
    cnt <- if(!is.null(rv$clean_data) && "Area" %in% names(rv$clean_data)) length(unique(rv$clean_data$Area)) else 0
    valueBox(cnt, "Coverage Areas", icon = icon("map-marked-alt"), color = "green")
  })
  output$home_kpi_status <- renderValueBox({
    st <- if(rv$is_preprocessed) "Preprocessed & Clean" else "Raw Dataset"
    valueBox(st, "Pipeline Status", icon = icon("cogs"), color = "yellow")
  })
  
  # ----------------------------------------------------------------------------
  # DATA COLLECTION EVENT HANDLERS
  # ----------------------------------------------------------------------------
  observeEvent(input$file_upload, {
    req(input$file_upload)
    tryCatch({
      uploaded_df <- load_custom_data(input$file_upload$datapath)
      val_res <- validate_crime_data(uploaded_df)
      rv$raw_data <- uploaded_df
      rv$validation <- val_res
      rv$is_preprocessed <- FALSE
      rv$clean_data <- NULL
      
      if (val_res$valid) {
        showNotification("CSV dataset loaded successfully! Proceed to preprocessing.", type = "message")
      } else {
        showNotification(val_res$message, type = "error", duration = 8)
      }
    }, error = function(e) {
      showNotification(paste("File loading error:", e$message), type = "error")
    })
  })
  
  observeEvent(input$btn_sample_data, {
    sample_df <- load_sample_data()
    rv$raw_data <- sample_df
    rv$validation <- validate_crime_data(sample_df)
    prep_res <- preprocess_data(sample_df)
    rv$clean_data <- prep_res$cleaned_data
    rv$before_stats <- prep_res$before_stats
    rv$after_stats <- prep_res$after_stats
    rv$cleaning_log <- prep_res$cleaning_log
    rv$is_preprocessed <- TRUE
    showNotification("Built-in sample dataset loaded and preprocessed!", type = "message")
  })
  
  output$validation_status_ui <- renderUI({
    req(rv$validation)
    if (rv$validation$valid) {
      div(class = "status-pill status-solved", style = "width:100%; padding:10px; font-size:12px;", icon("check-circle"), " ", rv$validation$message)
    } else {
      div(class = "status-pill status-unsolved", style = "width:100%; padding:10px; font-size:12px;", icon("exclamation-triangle"), " ", rv$validation$message)
    }
  })
  
  output$col_meta_records_txt <- renderText({
    info <- get_dataset_info(rv$raw_data)
    as.character(info$num_records)
  })
  output$col_meta_cols_txt <- renderText({
    info <- get_dataset_info(rv$raw_data)
    as.character(info$num_columns)
  })
  output$col_meta_size_txt <- renderText({
    info <- get_dataset_info(rv$raw_data)
    info$memory_size
  })
  output$col_meta_missing_txt <- renderText({
    info <- get_dataset_info(rv$raw_data)
    as.character(info$missing_cell_count)
  })
  
  # Retain standard valueBox outputs for compatibility
  output$col_meta_records <- renderValueBox({
    info <- get_dataset_info(rv$raw_data)
    valueBox(info$num_records, "Total Records", icon = icon("list"), color = "green")
  })
  output$col_meta_cols <- renderValueBox({
    info <- get_dataset_info(rv$raw_data)
    valueBox(info$num_columns, "Total Columns", icon = icon("columns"), color = "purple")
  })
  output$col_meta_size <- renderValueBox({
    info <- get_dataset_info(rv$raw_data)
    valueBox(info$memory_size, "Memory Size", icon = icon("hdd"), color = "blue")
  })
  output$col_meta_missing <- renderValueBox({
    info <- get_dataset_info(rv$raw_data)
    valueBox(info$missing_cell_count, "Missing Cells", icon = icon("exclamation-circle"), color = "yellow")
  })
  
  output$raw_column_names <- renderText({
    if (is.null(rv$raw_data)) return("No dataset loaded.")
    paste(colnames(rv$raw_data), collapse = " | ")
  })
  
  output$raw_data_table <- renderDT({
    req(rv$raw_data)
    datatable(rv$raw_data, options = list(pageLength = 8, scrollX = TRUE), style = "bootstrap4", class = "hover")
  })
  
  # ----------------------------------------------------------------------------
  # PREPROCESSING EVENT HANDLERS
  # ----------------------------------------------------------------------------
  output$before_prep_stats_ui <- renderUI({
    req(rv$raw_data)
    issues <- detect_data_issues(rv$raw_data)
    tags$ul(
      style = "font-size: 13px; color: #647875; line-height: 1.8;",
      tags$li(strong("Total Raw Records: "), issues$total_records),
      tags$li(strong("Total Missing Values: "), issues$missing_values),
      tags$li(strong("Duplicate Records Found: "), issues$duplicate_records)
    )
  })
  
  observeEvent(input$btn_preprocess, {
    req(rv$raw_data)
    if (!rv$validation$valid) {
      showNotification("Cannot preprocess invalid dataset. Fix schema errors first.", type = "error")
      return()
    }
    
    withProgress(message = "Cleaning dataset...", value = 0.5, {
      prep_res <- preprocess_data(rv$raw_data)
      rv$clean_data <- prep_res$cleaned_data
      rv$before_stats <- prep_res$before_stats
      rv$after_stats <- prep_res$after_stats
      rv$cleaning_log <- prep_res$cleaning_log
      rv$is_preprocessed <- TRUE
    })
    showNotification("Data preprocessing pipeline executed successfully!", type = "message")
  })
  
  output$preprocessing_log_output <- renderText({
    if (length(rv$cleaning_log) == 0) return("Pipeline not executed yet.")
    paste(rv$cleaning_log, collapse = "\n")
  })
  
  output$prep_meta_records_txt <- renderText({
    if (!is.null(rv$after_stats)) as.character(rv$after_stats$total_records) else "0"
  })
  output$prep_meta_missing_txt <- renderText({
    if (!is.null(rv$after_stats)) as.character(rv$after_stats$missing_values) else "0"
  })
  output$prep_meta_duplicates_txt <- renderText({
    if (!is.null(rv$after_stats)) as.character(rv$after_stats$duplicate_records) else "0"
  })
  
  output$clean_data_table <- renderDT({
    req(rv$clean_data)
    datatable(rv$clean_data, options = list(pageLength = 8, scrollX = TRUE), style = "bootstrap4", class = "hover")
  })
  
  # ----------------------------------------------------------------------------
  # DYNAMIC FILTER SELECTION UPDATES
  # ----------------------------------------------------------------------------
  observe({
    req(rv$clean_data)
    df <- rv$clean_data
    
    updateSelectInput(session, "filter_year", choices = c("All", sort(unique(df$Year))), selected = "All")
    updateSelectInput(session, "filter_city", choices = c("All", sort(unique(df$City))), selected = "All")
    updateSelectInput(session, "filter_area", choices = c("All", sort(unique(df$Area))), selected = "All")
    updateSelectInput(session, "filter_type", choices = c("All", sort(unique(df$Crime_Type))), selected = "All")
    updateSelectInput(session, "filter_category", choices = c("All", sort(unique(df$Crime_Category))), selected = "All")
    updateSelectInput(session, "filter_status", choices = c("All", sort(unique(df$Status))), selected = "All")
  })
  
  observeEvent(input$btn_reset_filters, {
    updateSelectInput(session, "filter_year", selected = "All")
    updateSelectInput(session, "filter_city", selected = "All")
    updateSelectInput(session, "filter_area", selected = "All")
    updateSelectInput(session, "filter_type", selected = "All")
    updateSelectInput(session, "filter_category", selected = "All")
    updateSelectInput(session, "filter_status", selected = "All")
  })
  
  # Filtered Data Reactive Function
  filtered_data <- reactive({
    df <- rv$clean_data
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    if (!is.null(input$filter_year) && input$filter_year != "All") {
      df <- df[!is.na(df$Year) & df$Year == as.numeric(input$filter_year), ]
    }
    if (!is.null(input$filter_city) && input$filter_city != "All") {
      df <- df[!is.na(df$City) & df$City == input$filter_city, ]
    }
    if (!is.null(input$filter_area) && input$filter_area != "All") {
      df <- df[!is.na(df$Area) & df$Area == input$filter_area, ]
    }
    if (!is.null(input$filter_type) && input$filter_type != "All") {
      df <- df[!is.na(df$Crime_Type) & df$Crime_Type == input$filter_type, ]
    }
    if (!is.null(input$filter_category) && input$filter_category != "All") {
      df <- df[!is.na(df$Crime_Category) & df$Crime_Category == input$filter_category, ]
    }
    if (!is.null(input$filter_status) && input$filter_status != "All") {
      df <- df[!is.na(df$Status) & df$Status == input$filter_status, ]
    }
    
    return(df)
  })
  
  # ----------------------------------------------------------------------------
  # RENDER VISUALIZATIONS (PLOTLY / LEAFLET)
  # ----------------------------------------------------------------------------
  output$plot_year_trend <- renderPlotly({
    df <- filtered_data()
    req(df)
    trends <- analyze_crime_trends(df)
    plot_yearly_trend(trends$by_year)
  })
  
  output$plot_month_dist <- renderPlotly({
    df <- filtered_data()
    req(df)
    trends <- analyze_crime_trends(df)
    plot_monthly_distribution(trends$by_month)
  })
  
  output$plot_dow_dist <- renderPlotly({
    df <- filtered_data()
    req(df)
    trends <- analyze_crime_trends(df)
    plot_day_of_week_distribution(trends$by_day_of_week)
  })
  
  output$plot_hour_trend <- renderPlotly({
    df <- filtered_data()
    req(df)
    trends <- analyze_crime_trends(df)
    plot_hourly_trend(trends$by_hour)
  })
  
  output$plot_type_dist <- renderPlotly({
    df <- filtered_data()
    req(df)
    types <- analyze_crime_types(df)
    plot_crime_type_distribution(types$type_table)
  })
  
  output$plot_cat_donut <- renderPlotly({
    df <- filtered_data()
    req(df)
    types <- analyze_crime_types(df)
    plot_crime_category_donut(types$category_table)
  })
  
  output$plot_area_dist <- renderPlotly({
    df <- filtered_data()
    req(df)
    areas <- analyze_areas(df)
    plot_crime_by_area(areas$area_summary)
  })
  
  output$plot_heatmap <- renderPlotly({
    df <- filtered_data()
    req(df)
    areas <- analyze_areas(df)
    plot_area_category_heatmap(areas$area_category_matrix)
  })
  
  output$plot_gender_donut <- renderPlotly({
    df <- filtered_data()
    req(df)
    victims <- analyze_victims(df)
    plot_victim_gender_donut(victims$gender_dist)
  })
  
  output$plot_age_hist <- renderPlotly({
    df <- filtered_data()
    req(df)
    plot_victim_age_histogram(df)
  })
  
  output$plot_weapon_bar <- renderPlotly({
    df <- filtered_data()
    req(df)
    weapons <- analyze_weapons(df)
    plot_weapon_usage(weapons$weapon_freq)
  })
  
  output$plot_status_donut <- renderPlotly({
    df <- filtered_data()
    req(df)
    st <- analyze_crime_status(df)
    plot_crime_status_donut(st)
  })
  
  output$plot_leaflet_map <- renderLeaflet({
    df <- filtered_data()
    req(df)
    plot_geographic_map(df)
  })
  
  # ----------------------------------------------------------------------------
  # REPORT GENERATION HANDLERS
  # ----------------------------------------------------------------------------
  observeEvent(input$btn_gen_report, {
    req(rv$clean_data)
    withProgress(message = "Generating Analytical Crime Report...", value = 0.5, {
      outfile <- file.path(tempdir(), input$report_filename)
      format_choice <- input$report_format
      
      gen_path <- generate_crime_report(rv$clean_data, output_file = outfile, format = format_choice)
      rv$generated_report_path <- gen_path
    })
    showNotification("Analytical report generated successfully!", type = "message")
  })
  
  output$report_status_msg <- renderUI({
    if (is.null(rv$generated_report_path)) {
      div(class = "status-pill status-investigation", style = "padding:10px 16px; font-size:12px; display:inline-block;", icon("info-circle"), " Click 'Generate Crime Analytics Report' to create your exportable report.")
    } else {
      div(class = "status-pill status-solved", style = "padding:10px 16px; font-size:12px; display:inline-block;", icon("check-circle"), " Report generated and ready for download: ", span(basename(rv$generated_report_path), style = "font-weight:bold;"))
    }
  })
  
  output$report_embedded_preview <- renderUI({
    req(rv$generated_report_path)
    if (file.exists(rv$generated_report_path) && grepl("\\.html$", rv$generated_report_path)) {
      lines <- readLines(rv$generated_report_path, warn = FALSE)
      HTML(paste(lines, collapse = "\n"))
    } else {
      p(class = "text-muted", "PDF report generated. Click 'Download Generated Report' to save file.")
    }
  })
  
  output$btn_download_report <- downloadHandler(
    filename = function() {
      if (!is.null(rv$generated_report_path)) {
        basename(rv$generated_report_path)
      } else {
        "crime_analytics_report.html"
      }
    },
    content = function(file) {
      if (!is.null(rv$generated_report_path) && file.exists(rv$generated_report_path)) {
        file.copy(rv$generated_report_path, file)
      } else {
        outfile <- generate_crime_report(rv$clean_data, output_file = file, format = "html")
      }
    }
  )
}

# Launch Shiny Application
shinyApp(ui = ui, server = server)
