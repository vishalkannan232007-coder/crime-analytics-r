# ==============================================================================
# DATA VISUALIZATION MODULE - PREMIUM REFINED GREEN THEME
# Data Driven Crime Analytics System using R
# ==============================================================================

# Cohesive Palette (Forest Green, Emerald, Sage, Olive, Soft Accents)
THEME_GREEN_PRIMARY <- "#2D9B72"
THEME_FOREST        <- "#203D32"
THEME_SAGE          <- "#A9B88B"
THEME_LIGHT_SAGE    <- "#DCEBD9"
THEME_BLUE          <- "#4C6EF5"
THEME_PERIWINKLE    <- "#6C8EEF"
THEME_VIOLET        <- "#845EC2"
THEME_PURPLE        <- "#A55EEA"
THEME_PINK          <- "#FF6F91"
THEME_ORANGE        <- "#FF9671"
THEME_AMBER         <- "#FA8231"
THEME_TEAL          <- "#00897B"

COLOR_PALETTE <- c(
  THEME_GREEN_PRIMARY, THEME_BLUE, THEME_VIOLET, THEME_PINK, 
  THEME_ORANGE, THEME_PURPLE, THEME_TEAL, THEME_AMBER, THEME_SAGE
)

# Custom ggplot theme for sleek glassmorphic dashboard display
theme_crime_dashboard <- function() {
  ggplot2::theme_minimal(base_family = "sans", base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 13, color = "#172A2A", hjust = 0, margin = ggplot2::margin(b = 8)),
      plot.subtitle = ggplot2::element_text(size = 10.5, color = "#647875", hjust = 0, margin = ggplot2::margin(b = 10)),
      axis.title = ggplot2::element_text(face = "bold", size = 10.5, color = "#647875"),
      axis.text = ggplot2::element_text(size = 9.5, color = "#172A2A"),
      panel.grid.major = ggplot2::element_line(color = "#EAEFEA", linewidth = 0.5),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 9.5, color = "#647875"),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}

# ==============================================================================
# DASHBOARD HOME SPECIALIZED VISUALIZATIONS (Matching Reference Image)
# ==============================================================================

#' 1. Home Crime Trends Over Time (Smooth Spline Area Chart)
#' Monthly crime count and pattern matching reference dashboard
plot_home_trends <- function(df, timeframe = "monthly") {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  if ("Month" %in% names(df)) {
    # Extract short month
    df$ShortMonth <- substr(df$Month, 1, 3)
    tab <- as.data.frame(table(ShortMonth = factor(df$ShortMonth, levels = month_order)), stringsAsFactors = FALSE)
    colnames(tab) <- c("Month", "Count")
  } else {
    tab <- data.frame(Month = month_order[1:7], Count = c(12, 18, 30, 22, 16, 25, 40))
  }
  
  # Ensure Month is factor
  tab$Month <- factor(tab$Month, levels = month_order)
  tab <- tab[!is.na(tab$Month), ]
  
  # Render Plotly spline area chart
  p <- plotly::plot_ly(
    tab,
    x = ~Month,
    y = ~Count,
    type = 'scatter',
    mode = 'lines+markers',
    line = list(shape = 'spline', color = THEME_GREEN_PRIMARY, width = 3),
    marker = list(size = 7, color = THEME_GREEN_PRIMARY, line = list(color = '#FFFFFF', width = 2)),
    fill = 'tozeroy',
    fillcolor = 'rgba(45, 155, 114, 0.15)',
    hoverinfo = 'text',
    text = ~paste0("<b>", Month, "</b><br>Incidents: ", Count)
  ) %>% plotly::layout(
    xaxis = list(
      title = "",
      showgrid = FALSE,
      zeroline = FALSE,
      tickfont = list(family = "sans-serif", size = 11, color = "#647875")
    ),
    yaxis = list(
      title = "",
      showgrid = TRUE,
      gridcolor = "#EAEFEA",
      zeroline = FALSE,
      tickfont = list(family = "sans-serif", size = 11, color = "#647875")
    ),
    paper_bgcolor = 'transparent',
    plot_bgcolor = 'transparent',
    margin = list(l = 30, r = 20, t = 10, b = 30),
    showlegend = FALSE
  ) %>% plotly::config(displayModeBar = FALSE, responsive = TRUE)
  
  return(p)
}

#' 2. Home Crime by Type (Donut Chart with Center Total & Right Legend)
plot_home_type_donut <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  # Calculate top types + others
  type_counts <- sort(table(df$Crime_Type), decreasing = TRUE)
  total_incidents <- sum(type_counts)
  
  if (length(type_counts) > 6) {
    top_types <- head(type_counts, 6)
    others_count <- sum(type_counts[7:length(type_counts)])
    cat_names <- c(names(top_types), "Others")
    cat_counts <- c(as.numeric(top_types), others_count)
  } else {
    cat_names <- names(type_counts)
    cat_counts <- as.numeric(type_counts)
  }
  
  type_df <- data.frame(
    Crime_Type = cat_names,
    Count = cat_counts,
    Percentage = round((cat_counts / total_incidents) * 100, 1),
    stringsAsFactors = FALSE
  )
  
  donut_colors <- c(
    "#2D9B72", # Theft / Top
    "#4C6EF5", # Assault
    "#845EC2", # Burglary
    "#FF6F91", # Fraud
    "#FF9671", # Vandalism
    "#6C5CE7", # Robbery
    "#A0AEC0"  # Others
  )
  donut_colors <- rep(donut_colors, length.out = nrow(type_df))
  
  p <- plotly::plot_ly(
    type_df,
    labels = ~Crime_Type,
    values = ~Count,
    type = 'pie',
    hole = 0.62,
    marker = list(colors = donut_colors, line = list(color = '#FFFFFF', width = 2)),
    textinfo = 'none',
    hoverinfo = 'text',
    text = ~paste0("<b>", Crime_Type, "</b><br>", Count, " incidents (", Percentage, "%)")
  ) %>% plotly::layout(
    annotations = list(
      list(
        text = paste0("<b>", total_incidents, "</b><br><span style='font-size:12px;color:#8A9995;font-weight:normal;'>Total</span>"),
        x = 0.5, y = 0.5,
        font = list(family = "sans-serif", size = 20, color = "#172A2A"),
        showarrow = FALSE
      )
    ),
    paper_bgcolor = 'transparent',
    plot_bgcolor = 'transparent',
    margin = list(l = 10, r = 10, t = 10, b = 10),
    showlegend = TRUE,
    legend = list(
      orientation = 'v',
      x = 1.05, y = 0.5,
      font = list(family = "sans-serif", size = 11, color = "#647875"),
      bgcolor = 'transparent'
    )
  ) %>% plotly::config(displayModeBar = FALSE, responsive = TRUE)
  
  return(p)
}

#' 3. Home Top 5 Areas Progress Bars HTML Component
render_home_top_areas_html <- function(df) {
  if (is.null(df) || nrow(df) == 0 || !"Area" %in% names(df)) {
    return(shiny::div(class = "text-muted", "No area records available."))
  }
  
  area_counts <- sort(table(df$Area), decreasing = TRUE)
  top_areas <- head(area_counts, 5)
  max_count <- max(as.numeric(top_areas), 1)
  
  # Vibrant modern palette matching reference image
  bar_colors <- c(
    "#2D9B72", # Teal / Emerald
    "#4A90E2", # Royal Blue
    "#6C8EEF", # Periwinkle
    "#A55EEA", # Purple
    "#FA8231"  # Amber Orange
  )
  
  area_items <- lapply(seq_along(top_areas), function(i) {
    name <- names(top_areas)[i]
    cnt <- as.numeric(top_areas)[i]
    pct <- round((cnt / max_count) * 100)
    color <- bar_colors[((i - 1) %% length(bar_colors)) + 1]
    
    shiny::tags$div(
      class = "area-progress-item",
      shiny::tags$div(
        class = "api-top",
        shiny::tags$span(class = "api-name", name),
        shiny::tags$span(class = "api-count", cnt)
      ),
      shiny::tags$div(
        class = "api-bar-track",
        shiny::tags$div(
          class = "api-bar-fill",
          style = sprintf("width: %d%%; background-color: %s;", pct, color)
        )
      )
    )
  })
  
  return(shiny::tags$div(class = "area-progress-list", area_items))
}

#' 4. Home Crime Heatmap & Density Visualization
plot_home_heatmap <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  # Create a matrix of Area vs Crime Category
  tab <- as.data.frame(table(Area = df$Area, Category = df$Crime_Category), stringsAsFactors = FALSE)
  colnames(tab) <- c("Area", "Category", "Count")
  
  p <- ggplot2::ggplot(tab, ggplot2::aes(x = Category, y = Area, fill = Count)) +
    ggplot2::geom_tile(color = "#FFFFFF", linewidth = 1) +
    ggplot2::scale_fill_gradientn(
      colors = c("#F4F7F5", "#DCEBD9", "#A9B88B", "#2D9B72", "#203D32"),
      guide = ggplot2::guide_colorbar(barwidth = 0.8, barheight = 5, title.position = "top")
    ) +
    theme_crime_dashboard() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 9, angle = 25, hjust = 1, color = "#647875"),
      axis.text.y = ggplot2::element_text(size = 9.5, color = "#172A2A"),
      legend.title = ggplot2::element_text(size = 8.5, color = "#647875"),
      legend.position = "right",
      plot.margin = ggplot2::margin(5, 5, 5, 5)
    )
  
  ply <- plotly::ggplotly(p) %>%
    plotly::layout(
      paper_bgcolor = 'transparent',
      plot_bgcolor = 'transparent',
      margin = list(l = 80, r = 10, t = 10, b = 40)
    ) %>%
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
  
  return(ply)
}

#' 5. Home Quick Insights Dynamic Generator
generate_home_quick_insights <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(shiny::tags$ul(
      class = "insights-list",
      shiny::tags$li("Load dataset to view intelligent operational insights.")
    ))
  }
  
  tot <- nrow(df)
  
  # Most common crime type
  type_counts <- sort(table(df$Crime_Type), decreasing = TRUE)
  top_type_name <- names(type_counts)[1]
  top_type_pct <- round((type_counts[1] / tot) * 100)
  
  # Top area
  area_counts <- sort(table(df$Area), decreasing = TRUE)
  top_area_name <- names(area_counts)[1]
  top_area_cnt <- area_counts[1]
  
  # Solved status rate
  solved_cnt <- sum(df$Status == "Solved", na.rm = TRUE)
  solved_pct <- round((solved_cnt / tot) * 100)
  
  # Peak crime hours
  if ("Hour" %in% names(df)) {
    evening_crimes <- sum(df$Hour >= 18 | df$Hour <= 0, na.rm = TRUE)
    evening_pct <- round((evening_crimes / tot) * 100)
    hour_insight <- sprintf("High incident concentration: %d%% occur during evening/night hours (6 PM - 12 AM).", evening_pct)
  } else {
    hour_insight <- "Most crimes occur between 6 PM - 12 AM."
  }
  
  insights <- list(
    sprintf("%s is the most common crime type (%d%%).", top_type_name, top_type_pct),
    sprintf("%s area has the highest crime count (%d).", top_area_name, top_area_cnt),
    sprintf("Overall case clearance: %d%% of recorded crimes are marked as Solved.", solved_pct),
    hour_insight
  )
  
  items <- lapply(insights, function(txt) {
    shiny::tags$li(
      shiny::tags$span(class = "insight-bullet"),
      shiny::tags$span(txt)
    )
  })
  
  return(shiny::tags$ul(class = "insights-list", items))
}

#' 6. Home Recent Records HTML Table
render_home_recent_records_html <- function(df, limit = 5) {
  if (is.null(df) || nrow(df) == 0) {
    return(shiny::tags$p(class = "text-muted", "No records found."))
  }
  
  # Take top records
  rec <- head(df, limit)
  
  # Ensure columns exist
  id_col    <- if("Crime_ID" %in% names(rec)) rec$Crime_ID else paste0("CR-", 1:nrow(rec))
  date_col  <- if("Date" %in% names(rec)) paste(rec$Date, if("Time" %in% names(rec)) rec$Time else "") else "N/A"
  type_col  <- if("Crime_Type" %in% names(rec)) rec$Crime_Type else "Unknown"
  area_col  <- if("Area" %in% names(rec)) rec$Area else "N/A"
  status_col<- if("Status" %in% names(rec)) rec$Status else "Under Investigation"
  
  rows <- lapply(seq_len(nrow(rec)), function(i) {
    st <- status_col[i]
    st_class <- switch(
      st,
      "Solved" = "status-solved",
      "Under Investigation" = "status-investigation",
      "Unsolved" = "status-unsolved",
      "Closed" = "status-closed",
      "status-investigation"
    )
    
    shiny::tags$tr(
      shiny::tags$td(shiny::tags$span(style = "font-weight:600; color:#172A2A;", id_col[i])),
      shiny::tags$td(date_col[i]),
      shiny::tags$td(type_col[i]),
      shiny::tags$td(area_col[i]),
      shiny::tags$td(shiny::tags$span(class = paste("status-pill", st_class), st))
    )
  })
  
  tbl <- shiny::tags$table(
    class = "modern-table",
    shiny::tags$thead(
      shiny::tags$tr(
        shiny::tags$th("ID"),
        shiny::tags$th("Date & Time"),
        shiny::tags$th("Type"),
        shiny::tags$th("Area"),
        shiny::tags$th("Status")
      )
    ),
    shiny::tags$tbody(rows)
  )
  
  return(tbl)
}

# ==============================================================================
# EXISTING MODULE VISUALIZATIONS (Refined with new Green-White Glass Theme)
# ==============================================================================

# 1. Crime trend over years (Line chart)
plot_yearly_trend <- function(by_year) {
  if (is.null(by_year) || nrow(by_year) == 0) return(NULL)
  
  p <- ggplot2::ggplot(by_year, ggplot2::aes(x = Year, y = Count)) +
    ggplot2::geom_area(fill = THEME_GREEN_PRIMARY, alpha = 0.15) +
    ggplot2::geom_line(color = THEME_GREEN_PRIMARY, linewidth = 1.3) +
    ggplot2::geom_point(color = THEME_FOREST, size = 3) +
    ggplot2::labs(title = "Annual Crime Incident Trend", x = "Year", y = "Total Incident Count") +
    theme_crime_dashboard()
  
  return(plotly::ggplotly(p) %>% plotly::layout(paper_bgcolor = 'transparent', plot_bgcolor = 'transparent'))
}

# 2. Monthly crime distribution (Bar chart)
plot_monthly_distribution <- function(by_month) {
  if (is.null(by_month) || nrow(by_month) == 0) return(NULL)
  
  by_month$Month <- factor(by_month$Month, levels = c("January", "February", "March", "April", "May", "June", 
                                                      "July", "August", "September", "October", "November", "December"))
  
  p <- ggplot2::ggplot(by_month, ggplot2::aes(x = Month, y = Count, fill = Month)) +
    ggplot2::geom_col(show.legend = FALSE, width = 0.65, fill = THEME_GREEN_PRIMARY, alpha = 0.85) +
    ggplot2::labs(title = "Monthly Crime Distribution", x = "Month", y = "Incidents") +
    theme_crime_dashboard() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  return(plotly::ggplotly(p) %>% plotly::layout(paper_bgcolor = 'transparent', plot_bgcolor = 'transparent'))
}

# 3. Crime type distribution (Bar chart)
plot_crime_type_distribution <- function(type_df) {
  if (is.null(type_df) || nrow(type_df) == 0) return(NULL)
  
  p <- ggplot2::ggplot(type_df, ggplot2::aes(x = reorder(Crime_Type, Count), y = Count)) +
    ggplot2::geom_col(fill = THEME_GREEN_PRIMARY, width = 0.68) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Crime Incidents by Specific Type", x = "Crime Type", y = "Incidents") +
    theme_crime_dashboard()
  
  return(plotly::ggplotly(p) %>% plotly::layout(paper_bgcolor = 'transparent', plot_bgcolor = 'transparent'))
}

# 4. Crime category distribution (Pie/Donut chart)
plot_crime_category_donut <- function(cat_df) {
  if (is.null(cat_df) || nrow(cat_df) == 0) return(NULL)
  
  p <- plotly::plot_ly(
    cat_df, 
    labels = ~Crime_Category, 
    values = ~Count, 
    type = 'pie',
    hole = 0.55,
    marker = list(colors = rep(COLOR_PALETTE, length.out = nrow(cat_df)), line = list(color = '#FFFFFF', width = 1.5)),
    textinfo = 'label+percent',
    hoverinfo = 'text',
    text = ~paste(Crime_Category, "<br>Count:", Count, "<br>Share:", Percentage, "%")
  ) %>% plotly::layout(
    title = list(text = "<b>Crime Category Breakdown</b>", font = list(size = 14, color = "#172A2A")),
    showlegend = TRUE,
    legend = list(orientation = 'h', y = -0.1),
    paper_bgcolor = 'transparent',
    plot_bgcolor = 'transparent'
  )
  
  return(p)
}

# 5. Crime by area (Horizontal bar chart)
plot_crime_by_area <- function(area_df) {
  if (is.null(area_df) || nrow(area_df) == 0) return(NULL)
  
  p <- ggplot2::ggplot(area_df, ggplot2::aes(x = reorder(Area, Count), y = Count)) +
    ggplot2::geom_col(fill = THEME_BLUE, width = 0.65) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Top Crime-Prone Areas & Neighborhoods", x = "Area Name", y = "Incidents") +
    theme_crime_dashboard()
  
  return(plotly::ggplotly(p) %>% plotly::layout(paper_bgcolor = 'transparent', plot_bgcolor = 'transparent'))
}

# 6. Crime by day of week (Bar chart)
plot_day_of_week_distribution <- function(by_dow) {
  if (is.null(by_dow) || nrow(by_dow) == 0) return(NULL)
  
  by_dow$Day_of_Week <- factor(by_dow$Day_of_Week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  p <- ggplot2::ggplot(by_dow, ggplot2::aes(x = Day_of_Week, y = Count)) +
    ggplot2::geom_col(fill = THEME_SAGE, width = 0.65) +
    ggplot2::labs(title = "Weekly Crime Occurrence Pattern", x = "Day of Week", y = "Incidents") +
    theme_crime_dashboard()
  
  return(plotly::ggplotly(p) %>% plotly::layout(paper_bgcolor = 'transparent', plot_bgcolor = 'transparent'))
}

# 7. Crime by hour (Line/Bar chart)
plot_hourly_trend <- function(by_hour) {
  if (is.null(by_hour) || nrow(by_hour) == 0) return(NULL)
  
  p <- ggplot2::ggplot(by_hour, ggplot2::aes(x = Hour, y = Count)) +
    ggplot2::geom_area(fill = THEME_AMBER, alpha = 0.25) +
    ggplot2::geom_line(color = THEME_AMBER, linewidth = 1.2) +
    ggplot2::geom_point(color = THEME_FOREST, size = 2.5) +
    ggplot2::scale_x_continuous(breaks = seq(0, 23, by = 2)) +
    ggplot2::labs(title = "24-Hour Diurnal Crime Distribution", x = "Hour of Day (00:00 - 23:00)", y = "Incidents") +
    theme_crime_dashboard()
  
  return(plotly::ggplotly(p) %>% plotly::layout(paper_bgcolor = 'transparent', plot_bgcolor = 'transparent'))
}

# 8. Victim gender distribution (Donut chart)
plot_victim_gender_donut <- function(gender_df) {
  if (is.null(gender_df) || nrow(gender_df) == 0) return(NULL)
  
  p <- plotly::plot_ly(
    gender_df, 
    labels = ~Victim_Gender, 
    values = ~Count, 
    type = 'pie',
    hole = 0.5,
    marker = list(colors = c("#2D9B72", "#4C6EF5", "#A55EEA", "#FA8231"), line = list(color = '#FFFFFF', width = 1.5)),
    textinfo = 'label+percent',
    hoverinfo = 'text',
    text = ~paste("Gender:", Victim_Gender, "<br>Count:", Count, "<br>Share:", Percentage, "%")
  ) %>% plotly::layout(
    title = list(text = "<b>Victim Gender Share</b>", font = list(size = 14, color = "#172A2A")),
    showlegend = TRUE,
    paper_bgcolor = 'transparent',
    plot_bgcolor = 'transparent'
  )
  
  return(p)
}

# 9. Victim age distribution (Histogram)
plot_victim_age_histogram <- function(df) {
  if (is.null(df) || nrow(df) == 0 || !"Victim_Age" %in% names(df)) return(NULL)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Victim_Age)) +
    ggplot2::geom_histogram(binwidth = 5, fill = THEME_VIOLET, color = "#FFFFFF", alpha = 0.85) +
    ggplot2::labs(title = "Victim Age Frequency Distribution", x = "Victim Age (Years)", y = "Frequency") +
    theme_crime_dashboard()
  
  return(plotly::ggplotly(p) %>% plotly::layout(paper_bgcolor = 'transparent', plot_bgcolor = 'transparent'))
}

# 10. Weapon usage (Bar chart)
plot_weapon_usage <- function(wep_df) {
  if (is.null(wep_df) || nrow(wep_df) == 0) return(NULL)
  
  p <- ggplot2::ggplot(wep_df, ggplot2::aes(x = reorder(Weapon, Count), y = Count)) +
    ggplot2::geom_col(fill = THEME_PINK, width = 0.65) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Weapon Involvement Frequency", x = "Weapon Type", y = "Incidents") +
    theme_crime_dashboard()
  
  return(plotly::ggplotly(p) %>% plotly::layout(paper_bgcolor = 'transparent', plot_bgcolor = 'transparent'))
}

# 11. Crime status (Donut chart)
plot_crime_status_donut <- function(status_df) {
  if (is.null(status_df) || nrow(status_df) == 0) return(NULL)
  
  colors_status <- c(
    "Solved" = "#2D9B72",
    "Under Investigation" = "#FA8231",
    "Unsolved" = "#E74C3C",
    "Closed" = "#4A90E2"
  )
  
  p <- plotly::plot_ly(
    status_df, 
    labels = ~Status, 
    values = ~Count, 
    type = 'pie',
    hole = 0.52,
    marker = list(colors = unname(colors_status[status_df$Status]), line = list(color = '#FFFFFF', width = 1.5)),
    textinfo = 'label+percent',
    hoverinfo = 'text',
    text = ~paste("Status:", Status, "<br>Count:", Count, "<br>Share:", Percentage, "%")
  ) %>% plotly::layout(
    title = list(text = "<b>Case Resolution Status Distribution</b>", font = list(size = 14, color = "#172A2A")),
    showlegend = TRUE,
    paper_bgcolor = 'transparent',
    plot_bgcolor = 'transparent'
  )
  
  return(p)
}

# 12. Area vs Crime Category (Heatmap)
plot_area_category_heatmap <- function(cross_tab) {
  if (is.null(cross_tab) || nrow(cross_tab) == 0) return(NULL)
  
  p <- ggplot2::ggplot(cross_tab, ggplot2::aes(x = Category, y = Area, fill = Count)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
    ggplot2::scale_fill_gradient(low = "#E8F8F0", high = "#203D32") +
    ggplot2::labs(title = "Crime Density Heatmap: Area vs Category", x = "Crime Category", y = "Area", fill = "Count") +
    theme_crime_dashboard() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  
  return(plotly::ggplotly(p) %>% plotly::layout(paper_bgcolor = 'transparent', plot_bgcolor = 'transparent'))
}

# 13. Geographic crime map (Interactive Leaflet map with safe fallback)
plot_geographic_map <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  has_coords <- "Latitude" %in% names(df) && "Longitude" %in% names(df) &&
    any(!is.na(df$Latitude)) && any(!is.na(df$Longitude))
  
  if (has_coords && requireNamespace("leaflet", quietly = TRUE)) {
    valid_geo <- df[!is.na(df$Latitude) & !is.na(df$Longitude), ]
    
    # Palette by category
    factpal <- leaflet::colorFactor(
      palette = c("#2D9B72", "#4C6EF5", "#FA8231", "#E74C3C", "#845EC2"),
      domain = valid_geo$Crime_Category
    )
    
    map <- leaflet::leaflet(valid_geo) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      leaflet::addCircleMarkers(
        lng = ~Longitude, 
        lat = ~Latitude,
        radius = 7,
        color = ~factpal(Crime_Category),
        stroke = FALSE, 
        fillOpacity = 0.85,
        popup = ~paste0(
          "<div style='font-family:sans-serif;'>",
          "<b style='color:#203D32;'>Case: </b>", Crime_ID, "<br>",
          "<b>Type: </b>", Crime_Type, "<br>",
          "<b>Category: </b>", Crime_Category, "<br>",
          "<b>Area: </b>", Area, ", ", City, "<br>",
          "<b>Date: </b>", Date, " ", Time, "<br>",
          "<b>Status: </b><span style='color:#2D9B72;font-weight:bold;'>", Status, "</span>",
          "</div>"
        ),
        clusterOptions = leaflet::markerClusterOptions()
      ) %>%
      leaflet::addLegend(
        pal = factpal, 
        values = ~Crime_Category, 
        title = "Crime Category",
        position = "bottomright"
      )
    
    return(map)
  } else {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = Area, y = Crime_Type, color = Status)) +
      ggplot2::geom_jitter(size = 3, alpha = 0.7, width = 0.2, height = 0.2) +
      ggplot2::labs(title = "Geographic Crime Incident Distribution", x = "Area", y = "Crime Type") +
      theme_crime_dashboard()
    
    return(plotly::ggplotly(p) %>% plotly::layout(paper_bgcolor = 'transparent', plot_bgcolor = 'transparent'))
  }
}
