# DATA DRIVEN CRIME ANALYTICS SYSTEM USING R

**Academic Final Year Project for B.E. / B.Tech Computer Science Engineering**  
**Domain:** Data Analytics | R Programming | Spatio-Temporal Data Visualization | Police Intelligence  

---

## 1. Executive Summary & Overview

The **Data Driven Crime Analytics System** is a complete, lightweight, web-based analytical dashboard built using **R** and **Shiny**. Designed to operate smoothly on standard laptops, the system enables law enforcement agencies, criminologists, and data analysts to upload raw crime datasets, automate data preprocessing and cleaning, compute statistical aggregations, explore dynamic spatio-temporal visualizations, and export comprehensive analytical reports.

---

## 2. Problem Statement

Modern law enforcement agencies collect massive quantities of incident records across cities and police beats. However, raw crime data is frequently fragmented, dirty, incomplete, and difficult to interpret in real time. Manual data inspection leads to delayed operational decision-making, sub-optimal police resource allocation, and missed hotspot identification. There is an urgent need for an automated, end-to-end, data-driven analytical tool that transforms uncleaned CSV incident logs into actionable intelligence.

---

## 3. Core Project Objectives

- **Automated Data Collection:** Allow users to upload custom CSV crime data or load built-in sample datasets with schema validation.
- **Data Quality Assurance & Cleaning:** Detect missing values, impute missing demographic attributes, remove duplicates, and derive temporal fields (`Year`, `Month`, `Day`, `Day_of_Week`, `Hour`, `Crime_Category`).
- **Multi-Dimensional Statistical Analysis:** Compute KPIs for crime types, geographical hotspots, victim demographics, weapon involvement, and case clearance rates.
- **Interactive Visualizations:** Provide 13 interactive charts, heatmaps, donut distributions, and geographic Leaflet hotspot maps.
- **Dynamic Report Generation:** Generate downloadable, publication-grade HTML and PDF analytical reports for law enforcement command staff.

---

## 4. System Architecture & Modules

```
crime-analytics-r/
│
├── app.R                       # Main R Shiny dashboard application entry point
├── README.md                   # Comprehensive academic project documentation
├── data/
│   └── sample_crime_data.csv   # Built-in realistic crime dataset (123+ records)
│
├── R/
│   ├── data_collection.R       # Data loading, CSV reading, schema validation
│   ├── preprocessing.R         # Imputation, deduplication, date/time parsing
│   ├── crime_analysis.R        # Statistical aggregations, KPI calculations
│   ├── visualization.R         # 13 ggplot2, Plotly, & Leaflet plot functions
│   └── report_generation.R     # RMarkdown & standalone HTML report generator
│
├── reports/
│   └── crime_report.Rmd        # RMarkdown template for automated reporting
│
└── www/
    └── custom.css              # Custom dashboard CSS styling and theme design
```

### Module Breakdown:

1. **Data Collection Module (`R/data_collection.R`):** CSV upload, column verification, metadata extraction (rows, columns, memory size, data types).
2. **Data Preprocessing Module (`R/preprocessing.R`):** Automated missing value imputation, duplicate row purge, ISO date parsing, time extraction, and categorical mapping.
3. **Crime Analysis Module (`R/crime_analysis.R`):** Trend analysis, crime type frequency, top area ranking, victim age/gender breakdown, weapon usage, and case status counts.
4. **Data Visualization Module (`R/visualization.R`):** Interactive Plotly charts (yearly trends, monthly bars, diurnal hours, donuts) and interactive Leaflet cluster maps.
5. **Report Generation Module (`R/report_generation.R`):** One-click report synthesis exporting self-contained HTML/PDF analytical documents.

---

## 5. Technology Stack

- **Primary Language:** R (v4.0+)
- **Web Application Framework:** Shiny, ShinyDashboard
- **Data Manipulation:** dplyr, readr, lubridate, tidyr
- **Visualizations:** ggplot2, Plotly, Leaflet, DT (DataTables)
- **Report Rendering:** RMarkdown, knitr, HTML5/CSS3

---

## 6. Functional & Non-Functional Requirements

### Functional Requirements:
- Upload raw CSV datasets or trigger built-in sample data.
- Validate required schema (`Crime_ID`, `Date`, `City`, `Area`, `Crime_Type`, `Crime_Category`, `Status`).
- Display before-and-after preprocessing audit logs.
- Dynamically filter visualizations by Year, City, Area, Crime Type, Category, and Status.
- Export dynamic analytical reports stamped with generation timestamp.

### Non-Functional Requirements:
- **Performance:** Lightweight execution requiring under 150 MB RAM.
- **Usability:** Modern, intuitive UI suitable for academic demonstrations.
- **Reliability:** Graceful error handling via `tryCatch()`, avoiding application crashes on missing columns or empty filters.
- **Offline Capability:** Operates 100% offline without external API keys.

---

## 7. Dataset Schema

The system expects CSV files matching or superset of the following schema:

| Column Name | Data Type | Description |
|---|---|---|
| `Crime_ID` | Character | Unique incident identifier (e.g., `CRM-2024-1001`) |
| `Date` | Date/String | Incident date (`YYYY-MM-DD`, `DD/MM/YYYY`) |
| `Year` | Numeric | Incident year |
| `Month` | Character | Full month name (`January`, `February`, etc.) |
| `Time` | String | 24-hour time string (`HH:MM`) |
| `City` | Character | City name |
| `Area` | Character | Neighborhood or precinct area |
| `Crime_Type` | Character | Specific crime classification (`Robbery`, `Burglary`, etc.) |
| `Crime_Category` | Character | High-level category (`Violent Crime`, `Property Crime`) |
| `Victim_Age` | Numeric | Victim age in years |
| `Victim_Gender` | Character | `Male`, `Female`, `Non-Binary`, `Unknown` |
| `Weapon` | Character | `Firearm`, `Knife`, `Crowbar`, `None`, etc. |
| `Status` | Character | `Solved`, `Under Investigation`, `Unsolved`, `Closed` |
| `Latitude` | Numeric | Geographic WGS84 latitude coordinate |
| `Longitude` | Numeric | Geographic WGS84 longitude coordinate |

---

## 8. Installation & Execution Guide

### Prerequisites:
1. Install **R** (version 4.0 or higher) from [CRAN](https://cran.r-project.org/).
2. (Optional) Install **RStudio Desktop**.

### Step 1: Install Required R Packages
Open R or RStudio console and run:

```R
install.packages(c(
  "shiny", "shinydashboard", "ggplot2", 
  "dplyr", "plotly", "DT", "leaflet", "rmarkdown"
))
```

### Step 2: Run the Application
Navigate to the project root directory and execute:

```R
library(shiny)
shiny::runApp("app.R")
```

Or click **"Run App"** inside RStudio.

---

## 9. Visualizations Included

1. **Annual Crime Incident Trend** (Line Chart)
2. **Monthly Crime Distribution** (Bar Chart)
3. **Crime Type Frequency** (Bar Chart)
4. **Crime Category Breakdown** (Interactive Donut Chart)
5. **Top Crime-Prone Areas** (Horizontal Bar Chart)
6. **Weekly Crime Occurrence Pattern** (Bar Chart)
7. **24-Hour Diurnal Crime Distribution** (Area Line Chart)
8. **Victim Gender Share** (Donut Chart)
9. **Victim Age Distribution** (Histogram)
10. **Weapon Usage Frequency** (Bar Chart)
11. **Case Clearance Resolution Status** (Donut Chart)
12. **Area vs Category Incident Density** (Tile Heatmap)
13. **Geographic Hotspot Map** (Interactive Leaflet Map with Marker Clusters)

---

## 10. Screenshots Section Placeholder

> *Placeholder: Insert screenshots of Dashboard Home, Data Preprocessing Audit, Interactive Visualization Panels, and Generated HTML Analytical Report for academic submission documentation.*

---

## 11. Future Enhancements

- **Predictive Machine Learning:** Integrate Time-Series Forecasting (ARIMA/Prophet) to predict future monthly incident rates.
- **Spatial Clustering:** Implement DBSCAN / K-Means algorithms for automated geographic hotspot polygon boundary detection.
- **Database Connectivity:** Connect Shiny application to PostgreSQL/PostGIS for real-time spatial data streams.

---

## 12. Academic Conclusion

The **Data Driven Crime Analytics System** demonstrates the high performance and versatility of R in data analytics, police intelligence, and interactive web dashboard development. By bridging the gap between raw CSV logs and dynamic graphical insights, the system empowers decision-makers to implement targeted crime prevention strategies.
