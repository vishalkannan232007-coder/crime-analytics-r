# ==============================================================================
# REPORT GENERATION MODULE
# Data Driven Crime Analytics System using R
# ==============================================================================

#' Generate comprehensive crime analytics report
#' @param clean_df Cleaned crime dataset
#' @param output_file Path where generated report will be saved
#' @param format "html" or "pdf"
#' @return Path to generated report file
generate_crime_report <- function(clean_df, output_file = "crime_analytics_report.html", format = "html") {
  if (is.null(clean_df) || nrow(clean_df) == 0) {
    stop("Cannot generate report for empty dataset.")
  }
  
  # Calculate summary metrics for report
  summary_kpi <- calculate_crime_summary(clean_df)
  trends <- analyze_crime_trends(clean_df)
  types <- analyze_crime_types(clean_df)
  areas <- analyze_areas(clean_df)
  victims <- analyze_victims(clean_df)
  weapons <- analyze_weapons(clean_df)
  status <- analyze_crime_status(clean_df)
  
  # Check if rmarkdown and pandoc are available
  rmd_template <- "reports/crime_report.Rmd"
  if (!file.exists(rmd_template)) {
    rmd_template <- system.file("reports", "crime_report.Rmd", package = "crimeanalytics")
  }
  
  can_use_rmd <- file.exists(rmd_template) && 
    requireNamespace("rmarkdown", quietly = TRUE) && 
    rmarkdown::pandoc_available()
  
  if (can_use_rmd) {
    tryCatch({
      env <- new.env()
      env$clean_df <- clean_df
      env$summary_kpi <- summary_kpi
      env$trends <- trends
      env$types <- types
      env$areas <- areas
      env$victims <- victims
      env$weapons <- weapons
      env$status <- status
      env$gen_time <- Sys.time()
      
      output_format <- if (format == "pdf") "pdf_document" else "html_document"
      
      rmarkdown::render(
        input = rmd_template,
        output_format = output_format,
        output_file = basename(output_file),
        output_dir = dirname(output_file),
        envir = env,
        quiet = TRUE
      )
      return(output_file)
    }, error = function(e) {
      message("RMarkdown render note: ", e$message, ". Falling back to standalone HTML report generator.")
      return(generate_standalone_html_report(clean_df, summary_kpi, trends, types, areas, victims, weapons, status, output_file))
    })
  } else {
    # Reliable Standalone HTML Generator
    return(generate_standalone_html_report(clean_df, summary_kpi, trends, types, areas, victims, weapons, status, output_file))
  }
}

#' Reliable Standalone HTML Report Generator
generate_standalone_html_report <- function(clean_df, summary_kpi, trends, types, areas, victims, weapons, status, output_file) {
  gen_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  
  # Build HTML content
  html_content <- paste0('
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Crime Analytics Report</title>
    <style>
        body { font-family: "Segoe UI", Roboto, Helvetica, Arial, sans-serif; margin: 40px; background-color: #F8F9FA; color: #2C3E50; }
        .header { background: linear-gradient(135deg, #1E3C72 0%, #2A5298 100%); color: white; padding: 30px; border-radius: 10px; margin-bottom: 30px; box-shadow: 0 4px 15px rgba(0,0,0,0.1); }
        .header h1 { margin: 0 0 10px 0; font-size: 28px; }
        .header p { margin: 5px 0 0 0; font-size: 14px; opacity: 0.9; }
        .card-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px; margin-bottom: 30px; }
        .kpi-card { background: white; padding: 20px; border-radius: 8px; border-left: 5px solid #2A5298; box-shadow: 0 2px 8px rgba(0,0,0,0.05); }
        .kpi-title { font-size: 12px; font-weight: bold; color: #7F8C8D; text-transform: uppercase; }
        .kpi-value { font-size: 24px; font-weight: bold; color: #2C3E50; margin-top: 5px; }
        .section { background: white; padding: 25px; border-radius: 8px; margin-bottom: 25px; box-shadow: 0 2px 8px rgba(0,0,0,0.05); }
        .section h2 { border-bottom: 2px solid #ECF0F1; padding-bottom: 10px; color: #1E3C72; margin-top: 0; font-size: 20px; }
        table { width: 100%; border-collapse: collapse; margin-top: 15px; }
        th { background-color: #2A5298; color: white; text-align: left; padding: 10px; font-size: 13px; }
        td { padding: 10px; border-bottom: 1px solid #ECF0F1; font-size: 13px; }
        tr:nth-child(even) { background-color: #F8F9FA; }
        .badge { display: inline-block; padding: 4px 8px; border-radius: 4px; font-size: 12px; font-weight: bold; color: white; background-color: #27AE60; }
        .footer { text-align: center; margin-top: 40px; font-size: 12px; color: #95A5A6; }
    </style>
</head>
<body>
    <div class="header">
        <h1>DATA DRIVEN CRIME ANALYTICS SYSTEM</h1>
        <p>Comprehensive Academic Analytical Report | B.E. / B.Tech Computer Science Project</p>
        <p><strong>Generated On:</strong> ', gen_time, '</p>
    </div>

    <div class="card-grid">
        <div class="kpi-card">
            <div class="kpi-title">Total Crimes Analyzed</div>
            <div class="kpi-value">', summary_kpi$total_crimes, '</div>
        </div>
        <div class="kpi-card">
            <div class="kpi-title">Most Common Crime Type</div>
            <div class="kpi-value">', summary_kpi$most_common_type, '</div>
        </div>
        <div class="kpi-card">
            <div class="kpi-title">Most Affected Area</div>
            <div class="kpi-value">', summary_kpi$most_affected_area, '</div>
        </div>
        <div class="kpi-card">
            <div class="kpi-title">Case Solved Rate</div>
            <div class="kpi-value">', summary_kpi$solved_rate, '</div>
        </div>
    </div>

    <div class="section">
        <h2>1. Executive Project Summary</h2>
        <p>This data-driven crime analytics report synthesizes empirical statistics collected and preprocessed using R. The system evaluates crime distribution patterns across temporal, spatial, demographic, and weapon-use dimensions to assist law enforcement agencies in strategic resource deployment.</p>
    </div>

    <div class="section">
        <h2>2. Data Preprocessing & Quality Assurance</h2>
        <p>The input raw dataset underwent automated missing value imputation, duplicate removal, date/time standardization, and categorical alignment. Preprocessing verified schema integrity across <strong>', nrow(clean_df), '</strong> clean crime records.</p>
    </div>

    <div class="section">
        <h2>3. Top Crime Types Breakdown</h2>
        <table>
            <thead>
                <tr>
                    <th>Crime Type</th>
                    <th>Incident Count</th>
                    <th>Percentage Share (%)</th>
                </tr>
            </thead>
            <tbody>
', paste(sapply(1:min(8, nrow(types$type_table)), function(i) {
    paste0("<tr><td>", types$type_table$Crime_Type[i], "</td><td>", types$type_table$Count[i], "</td><td>", types$type_table$Percentage[i], "%</td></tr>")
}), collapse = "\n"), '
            </tbody>
        </table>
    </div>

    <div class="section">
        <h2>4. Top Crime-Prone Areas</h2>
        <table>
            <thead>
                <tr>
                    <th>Area Name</th>
                    <th>Incident Count</th>
                    <th>Percentage Share (%)</th>
                </tr>
            </thead>
            <tbody>
', paste(sapply(1:min(8, nrow(areas$area_summary)), function(i) {
    paste0("<tr><td>", areas$area_summary$Area[i], "</td><td>", areas$area_summary$Count[i], "</td><td>", areas$area_summary$Percentage[i], "%</td></tr>")
}), collapse = "\n"), '
            </tbody>
        </table>
    </div>

    <div class="section">
        <h2>5. Victim Demographics & Age Analysis</h2>
        <p>Mean Victim Age: <strong>', victims$age_stats$mean, ' years</strong> | Median Age: <strong>', victims$age_stats$median, ' years</strong></p>
        <table>
            <thead>
                <tr>
                    <th>Age Group</th>
                    <th>Incident Count</th>
                    <th>Percentage Share (%)</th>
                </tr>
            </thead>
            <tbody>
', paste(sapply(1:nrow(victims$age_group_dist), function(i) {
    paste0("<tr><td>", victims$age_group_dist$Age_Group[i], "</td><td>", victims$age_group_dist$Count[i], "</td><td>", victims$age_group_dist$Percentage[i], "%</td></tr>")
}), collapse = "\n"), '
            </tbody>
        </table>
    </div>

    <div class="section">
        <h2>6. Weapon Involvement Frequency</h2>
        <table>
            <thead>
                <tr>
                    <th>Weapon Type</th>
                    <th>Incident Count</th>
                    <th>Percentage Share (%)</th>
                </tr>
            </thead>
            <tbody>
', paste(sapply(1:nrow(weapons$weapon_freq), function(i) {
    paste0("<tr><td>", weapons$weapon_freq$Weapon[i], "</td><td>", weapons$weapon_freq$Count[i], "</td><td>", weapons$weapon_freq$Percentage[i], "%</td></tr>")
}), collapse = "\n"), '
            </tbody>
        </table>
    </div>

    <div class="section">
        <h2>7. Case Resolution Status</h2>
        <table>
            <thead>
                <tr>
                    <th>Status</th>
                    <th>Count</th>
                    <th>Percentage Share (%)</th>
                </tr>
            </thead>
            <tbody>
', paste(sapply(1:nrow(status), function(i) {
    paste0("<tr><td>", status$Status[i], "</td><td>", status$Count[i], "</td><td>", status$Percentage[i], "%</td></tr>")
}), collapse = "\n"), '
            </tbody>
        </table>
    </div>

    <div class="section">
        <h2>8. Key Analytical Observations</h2>
        <ul>
            <li><strong>Dominant Offense Category:</strong> ', summary_kpi$most_common_category, ' accounts for the largest proportion of recorded incidents.</li>
            <li><strong>High-Risk Hotspot:</strong> Neighborhoods in ', summary_kpi$most_affected_area, ' exhibit peak incident densities requiring targeted patrol beats.</li>
            <li><strong>Investigation Efficiency:</strong> Overall case clearance rate stands at ', summary_kpi$solved_rate, '.</li>
        </ul>
    </div>

    <div class="section">
        <h2>9. Academic Conclusion</h2>
        <p>The Data Driven Crime Analytics System successfully demonstrates the applicability of R programming, automated data cleaning pipelines, interactive visualization, and reactive dashboard reporting for crime pattern discovery and operational decision support.</p>
    </div>

    <div class="footer">
        <p>Data Driven Crime Analytics System &copy; ', format(Sys.Date(), "%Y"), ' | Created using R & Shiny Framework</p>
    </div>
</body>
</html>
  ')
  
  writeLines(html_content, output_file)
  return(output_file)
}
