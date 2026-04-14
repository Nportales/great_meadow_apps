#### Hydrology R Shiny Dashboard #### 

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

#-------------------------------------------#
####    Read & Prepare Processed Data    ####
#-------------------------------------------#

# For data exploration, app building, or debugging, temporarily set working directory to app folder
# setwd("hydrology_app")

# Great Meadow data
gm <- read.csv("data/great_meadow_well_data_2025_20260304.csv") %>%
  mutate(date = as.Date(date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         ),
         siteyear = paste(site, year, sep = "_"))

# Gilmore Meadow data 
gl <- read.csv("data/gl_well_data_2025_20260127.csv") %>%
  mutate(date = as.Date(date),
         timestamp = as_datetime(timestamp),
         site = "Gilmore Meadow 1")

# Create precipitation lookup and combine datasets
gm_precip_lookup <- gm %>% select(timestamp, precip.cm, lag.precip) %>% distinct()

gl_with_gm_precip <- gl %>%
  select(-precip.cm, -lag.precip) %>%
  left_join(gm_precip_lookup, by = "timestamp") %>%
  left_join(gl %>% select(timestamp, orig_precip_cm = precip.cm, orig_lag_precip = lag.precip), 
            by = "timestamp") %>%
  mutate(precip.cm = coalesce(precip.cm, orig_precip_cm),
         lag.precip = coalesce(lag.precip, orig_lag_precip)) %>%
  select(-orig_precip_cm, -orig_lag_precip)

# Combine datasets
all_data <- bind_rows(gm, gl_with_gm_precip) %>% 
  filter(year >= 2016 & year(Sys.Date())) %>% 
  select(timestamp, date, year, doy, hr, doy_h, precip_cm = precip.cm,
         lag_precip = lag.precip, water_depth = water.depth, site)

# Water level stats
wl_stats <- read.csv("data/gm_gl_wl_stats_2025_20260304.csv") %>% 
  select(year, stat, `Gilmore Meadow 1` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year) %>% 
  mutate(wetland = if_else(grepl("Great Meadow", site), "Great Meadow", "Gilmore Meadow"))

#-----------------------#
####    Constants    ####
#-----------------------#

# Site color palette (defined once)
SITE_COLORS <- c(
  "Great Meadow 1" = "#000066", "Great Meadow 2" = "darkgreen", "Great Meadow 3" = "green",
  "Great Meadow 4" = "darkorange", "Great Meadow 5" = "deeppink2", "Great Meadow 6" = "purple",
  "Gilmore Meadow 1" = "chocolate4",   "Great Meadow (Average)" = "black",
  "Gilmore Meadow (Average)" = "darkgray", "Precipitation" = "blue"
)

# Variable name mapping for significance testing
VAR_MAPPING <- c(
  "WL_mean" = "Mean Water Level (cm)",
  "WL_sd" = "SD Water Level (cm)", 
  "WL_min" = "Minimum Water Level (cm)",
  "WL_max" = "Maximum Water Level (cm)",
  "max_inc" = "Maximum Hourly Increase (cm)",
  "max_dec" = "Maximum Hourly Decrease (cm)",
  "GS_change" = "Growing Season Change (cm)",
  "prop_over_0cm" = "GS % Surface Water",
  "prop_bet_0_neg30cm" = "GS % Within 30cm",
  "prop_under_neg30cm" = "GS % Over 30cm Deep"
)

# Common pickerInput options
PICKER_OPTIONS <- list(
  `actions-box` = TRUE,
  `deselect-all-text` = "Clear all",
  `select-all-text` = "Select all",
  `live-search` = TRUE,
  style = "btn-outline-primary"
)

#-----------------------#
####    Functions    ####
#-----------------------#

# hydrograph plot creation function
create_hydrograph_plot <- function(data) {
  req(nrow(data) > 0)
  
  minWL <- min(data$water_depth, na.rm = TRUE)
  sites <- unique(data$site)
  
  ggplot(data, aes(x = doy_h, y = water_depth)) +
    geom_line(aes(color = site), size = 0.7) +
    geom_line(aes(x = doy_h, y = lag_precip * 5 + minWL, color = "Precipitation"), size = 0.7) +
    geom_hline(yintercept = 0, color = 'brown') +
    facet_wrap(~ year, ncol = 1) +
    scale_color_manual(values = SITE_COLORS, breaks = c(sites, "Precipitation")) +
    labs(y = 'Water Level (cm)', x = 'Date') +
    scale_x_continuous(
      breaks = c(121, 152, 182, 213, 244, 274),
      labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(~ ., breaks = c(minWL, minWL + 10),
                          name = 'Hourly Precip. (cm)', labels = c('0', '2'))
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.text.y.right = element_text(color = 'blue'),
      axis.title.y.right = element_text(color = 'blue'),
      strip.text = element_text(size = 11),
      legend.position = "bottom",
      legend.title = element_blank()
    )
}

# statistical significance function
calculate_wetland_significance <- function(data, selected_years, selected_sites, alpha = 0.05) {
  if (length(selected_years) <= 3) {
    message("⚠️ Not enough years selected for significance testing (need >3).")
    return(NULL)
  }
  
  filtered_data <- data %>%
    filter(year %in% selected_years, site %in% selected_sites) %>%
    mutate(site_group = if_else(grepl("Great Meadow", site), "Great Meadow", "Gilmore Meadow"))
  
  wetlands_present <- unique(filtered_data$site_group)
  if (length(wetlands_present) < 2 || !all(c("Great Meadow", "Gilmore Meadow") %in% wetlands_present)) {
    message("⚠️ Both Great Meadow and Gilmore Meadow must be present for comparison.")
    return(NULL)
  }
  
  stat_cols <- filtered_data %>% select(where(is.numeric), -year) %>% names()
  
  yearly_means <- filtered_data %>%
    group_by(year, site_group) %>%
    summarise(across(all_of(stat_cols), \(x) mean(x, na.rm = TRUE)), .groups = "drop")
  
  map_dfr(stat_cols, function(var) {
    tryCatch({
      test <- t.test(as.formula(paste(var, "~ site_group")), data = yearly_means)
      data.frame(variable = var, p_value = test$p.value, significant = test$p.value < alpha)
    }, error = function(e) {
      data.frame(variable = var, p_value = NA, significant = FALSE)
    })
  })
}

# picker tool function
create_picker_input <- function(id, label, choices, selected, multiple = TRUE, none_text = "Choose options") {
  pickerInput(id, label = div(icon(if(id %in% c("selected_sites", "stats_site")) "map-marker" else "calendar"), label),
              choices = choices, selected = selected, multiple = multiple,
              options = c(PICKER_OPTIONS, list(`none-selected-text` = none_text)))
}

#----------------#
####    UI    ####
#----------------#

ui <- page_fluid(
  theme = bs_theme(
    version = 5, bootswatch = "flatly", primary = "#1B365D", secondary = "#4C6D9A",    
    success = "#2E86C1", info = "#3498db", warning = "#f39c12", danger = "#e74c3c",
    base_font = font_google("Open Sans"), heading_font = font_google("Open Sans", wght = c(400, 700))
  ),
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      .content-section {
        margin: 30px 0; padding: 25px; border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border: 2px solid #1B365D;
      }
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px; padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08); border: 1px solid #dee2e6;
      }
      .main-title {
        background: linear-gradient(135deg, #1B365D 0%, #4C6D9A 100%);
        color: white; padding: 30px; margin: -15px -15px 30px -15px;
        text-align: center; border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px; padding: 20px; margin: 20px 0;
        border: 1px solid #4C6D9A;
      }
      .dataTables_wrapper { font-size: 0.85rem !important; }
      .dataTables_wrapper table { font-size: 0.8rem !important; }
      .significance-info h5 {
        background-color: #fff3cd; color: #856404;
        padding: 6px 10px; border-radius: 4px;
        display: inline-block; margin-bottom: 10px;
      }
    "))
  ),
  
  # Main title
  div(class = "main-title",
      h1("Wetland Hydrology Dashboard", 
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # FIRST SECTION: Hydrographs section
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom", width = 300,
          h4("Hydrograph Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          create_picker_input(
            "selected_sites", "Select Site(s):", 
            list(
              "Individual Sites" = sort(unique(all_data$site)),
              "Wetland Averages" = c(
                "Gilmore Meadow (Average)",
                "Great Meadow (Average)"
              )
            ),
            "Great Meadow 1",
            none_text = "Choose site(s)"
          ),
          
          create_picker_input("year", "Select Year(s):", 
                              unique(all_data$year), 
                              selected = max(all_data$year),
                              none_text = "Choose year(s)"),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool (+) by clicking and dragging with your cursor to select data on the hydrograph and view below.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 10px; text-align: center;",
              downloadButton("download_plot", "Download Hydrograph", 
                             class = "btn-primary btn-sm", icon = icon("image"))),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_brush", "Download Selected Data", 
                             class = "btn-primary btn-sm", icon = icon("download"))),
          
          div(style = "margin-top: 15px; text-align: center;",
                tags$a(href = "#about",
                       class = "btn btn-primary btn-sm", icon("info-circle"),
                       "About")),
        ),
        
        card(
          full_screen = TRUE,
          card_header(class = "bg-primary text-white", "Hydrographs by Year"),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#4C6D9A", opacity = 0.3))
        )
      )
  ),
  
  # Selected data section
  div(class = "brush-info-section",
      card(
        card_header(class = "bg-success text-white", "Selected Data from Hydrograph:"),
        tableOutput("brush_info")
      )
  ),
  
  # SECOND SECTION: Water Level Stats
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom", width = 300,
          h4("Statistics Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          create_picker_input("stats_site", "Select Site(s):", 
                              unique(wl_stats$site), 
                              c("Great Meadow 1", "Gilmore Meadow 1"),
                              none_text = "Choose site(s)"),
          
          create_picker_input("stats_year", "Select Years:", 
                              unique(wl_stats$year), 
                              selected = tail(sort(unique(wl_stats$year)), 4),
                              none_text = "Choose year(s)"),
          
          radioButtons("time_summary", "Summarize Water Level Statistics By:",
                       choices = c("Each Year" = "year",
                                   "Average Across Years" = "multi",
                                   "All Sites (with statistical significance)" = "all_sites"),
                       selected = "year"),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_stats", "Download Table", 
                             class = "btn-primary btn-sm", icon = icon("download"))),
          
          div(style = "margin-top: 15px; text-align: center;",
              tags$a(href = "#about",
                     class = "btn btn-primary btn-sm", icon("info-circle"),
                     "About")),
        ),
        
        card(
          full_screen = TRUE,
          card_header(class = "bg-primary text-white", "Growing Season Water Level Statistics"),
          div(style = "padding: 10px;",
              uiOutput("significance_info"),
              dataTableOutput("wl_stats"))
        )
      )
  ),
  
  # About section
  div(id = "about",
    class = "brush-info-section",
      card(
        card_header(class = "bg-success text-white", "About"),
        includeHTML("./www/About.html")
      )
  )
  
)


#--------------------#
####    SERVER    ####
#--------------------#

server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$selected_sites)
    
    data <- all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275)
    
    selected <- input$selected_sites
    
    # ---- Separate selections ----
    real_sites <- selected[selected %in% unique(all_data$site)]
    avg_sites  <- selected[grepl("\\(Average\\)", selected)]
    
    # ---- Real site data ----
    real_data <- data %>%
      filter(site %in% real_sites)
    
    # ---- Average data ----
    avg_data <- map_dfr(avg_sites, function(avg_name) {
      
      wetland_name <- if (grepl("Great Meadow", avg_name)) {
        "Great Meadow"
      } else {
        "Gilmore Meadow"
      }
      
      data %>%
        filter(grepl(wetland_name, site)) %>%
        group_by(year, doy_h, timestamp) %>%
        summarise(
          water_depth = mean(water_depth, na.rm = TRUE),
          lag_precip = mean(lag_precip, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(site = avg_name)
    })
    
    bind_rows(real_data, avg_data)
  })
  
  # Render hydrograph plot
  output$hydrograph <- renderPlot({
    create_hydrograph_plot(plot_data())
  })
  
  # Processed brushed data
  processed_brush_data <- reactive({
    req(input$hydro_brush)
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    selected_data <- brushedPoints(plot_data_filtered, brush = input$hydro_brush,
                                   xvar = "doy_h", yvar = "water_depth") %>%
      select(timestamp, year, doy_h, site, water_depth, lag_precip) %>%
      arrange(year, doy_h) %>%
      mutate(across(c(doy_h, water_depth), ~ round(.x, 2)),
             lag_precip = round(lag_precip, 3),
             timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S"))
    
    if (nrow(selected_data) == 0) {
      return(data.frame(Message = "No points selected - try brushing over the water level lines"))
    }
    
    # Create base data and pivot water depths by site
    base_data <- selected_data %>%
      group_by(year, doy_h, timestamp, lag_precip) %>%
      summarise(.groups = 'drop') %>%
      arrange(year, doy_h)
    
    water_depth_data <- selected_data %>%
      group_by(year, doy_h, timestamp, site) %>%
      summarise(water_depth = mean(water_depth, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = site, values_from = water_depth)
    
    result_data <- base_data %>%
      left_join(water_depth_data, by = c("year", "doy_h", "timestamp")) %>%
      rename(Year = year, Timestamp = timestamp, `Day of Year` = doy_h, 
             `Precipitation (cm)` = lag_precip)
    
    # Add "Water Depth (cm)" suffix to site columns
    site_columns <- intersect(names(result_data), input$selected_sites)
    if (length(site_columns) > 0) {
      result_data <- result_data %>%
        rename_with(~ paste(.x, "Water Depth (cm)"), .cols = all_of(site_columns))
    }
    
    result_data %>% arrange(Year, `Day of Year`)
  })
  
  # Significance testing results
  significance_results <- reactive({
    req(input$stats_year, input$stats_site)
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    if (length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands)) {
      calculate_wetland_significance(wl_stats, input$stats_year, input$stats_site, alpha = 0.05)
    } else {
      NULL
    }
  })
  
  # Check if we should show significance info
  show_significance_info <- reactive({
    req(input$stats_site, input$time_summary)
    if (input$time_summary != "all_sites") return(FALSE)
    
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands)
  })
  
  # Filtered statistics data
  filtered_stats <- reactive({
    req(input$stats_site, input$stats_year, input$time_summary)
    
    data <- wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year)
    
    switch(input$time_summary,
           "year" = {
             # Per-Year Summary
             data %>%
               mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
               select(Year = year, Site = site, `Mean Water Level (cm)` = WL_mean,
                      `SD Water Level (cm)` = WL_sd, `Minimum Water Level (cm)` = WL_min,
                      `Maximum Water Level (cm)` = WL_max, `Maximum Hourly Increase (cm)` = max_inc,
                      `Maximum Hourly Decrease (cm)` = max_dec, `Growing Season Change (cm)` = GS_change,
                      `GS % Surface Water` = prop_over_0cm, `GS % Within 30cm` = prop_bet_0_neg30cm,
                      `GS % Over 30cm Deep` = prop_under_neg30cm)
           },
           "multi" = {
             # Average Across Years
             data %>%
               group_by(site, wetland) %>%
               summarise(
                 Year = paste0(min(year), "–", max(year)),
                 across(c(WL_mean, WL_sd, WL_min, WL_max, max_inc, max_dec, GS_change,
                          prop_over_0cm, prop_bet_0_neg30cm, prop_under_neg30cm), 
                        ~ round(mean(.x, na.rm = TRUE), 2)),
                 .groups = "drop"
               ) %>%
               rename(Site = site, Wetland = wetland, `Mean Water Level (cm)` = WL_mean,
                      `SD Water Level (cm)` = WL_sd, `Minimum Water Level (cm)` = WL_min,
                      `Maximum Water Level (cm)` = WL_max, `Maximum Hourly Increase (cm)` = max_inc,
                      `Maximum Hourly Decrease (cm)` = max_dec, `Growing Season Change (cm)` = GS_change,
                      `GS % Surface Water` = prop_over_0cm, `GS % Within 30cm` = prop_bet_0_neg30cm,
                      `GS % Over 30cm Deep` = prop_under_neg30cm)
           },
           "all_sites" = {
             # All Sites with significance
             data %>%
               group_by(wetland) %>%
               summarise(
                 site = "All Sites",
                 Year = paste0(min(year), "–", max(year)),
                 across(c(WL_mean, WL_sd, WL_min, WL_max, max_inc, max_dec, GS_change,
                          prop_over_0cm, prop_bet_0_neg30cm, prop_under_neg30cm), 
                        ~ round(mean(.x, na.rm = TRUE), 2)),
                 .groups = "drop"
               ) %>%
               rename(Site = site, Wetland = wetland, `Mean Water Level (cm)` = WL_mean,
                      `SD Water Level (cm)` = WL_sd, `Minimum Water Level (cm)` = WL_min,
                      `Maximum Water Level (cm)` = WL_max, `Maximum Hourly Increase (cm)` = max_inc,
                      `Maximum Hourly Decrease (cm)` = max_dec, `Growing Season Change (cm)` = GS_change,
                      `GS % Surface Water` = prop_over_0cm, `GS % Within 30cm` = prop_bet_0_neg30cm,
                      `GS % Over 30cm Deep` = prop_under_neg30cm)
           }
    )
  })
  
  # Render brush info table
  output$brush_info <- renderTable({
    processed_brush_data()
  })
  
  # Render significance info UI
  output$significance_info <- renderUI({
    req(input$time_summary == "all_sites")
    
    base_note <- HTML("
    <div style='background-color:#f9f9f9; padding:10px; border-left:4px solid #1B365D; margin-bottom:10px; font-size:13px;'>
      <strong>Note:</strong><br>
      • To compare sites, both Great Meadow and Gilmore Meadow must be selected.<br>
      • Significance testing requires more than three years of data.<br>
      • If fewer than three years are chosen, results display without significance testing.
    </div>")
    
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        sig_vars <- sig_results %>% filter(significant) %>% pull(variable)
        
        if (length(sig_vars) > 0) {
          sig_display_names <- VAR_MAPPING[sig_vars]
          sig_display_names <- sig_display_names[!is.na(sig_display_names)]
          
          return(tagList(
            base_note,
            div(class = "significance-info", style = "margin-bottom: 25px;",
                h5(icon("asterisk"), " Statistical Significance"),
                p("Yellow highlighted variables show significant differences (p < 0.05) between Great Meadow and Gilmore Meadow wetlands:"),
                tags$ul(lapply(sig_display_names, function(name) tags$li(name))),
                p(class = "note", "Tests compare averages between each wetland for all selected years and sites.",
                  style = "margin-top: 5px; font-size: 0.8rem; font-style: italic; color: #6c757d;")
            )
          ))
        }
      }
    }
    
    return(base_note)
  })
  
  # Render water level stats table with significance highlighting
  output$wl_stats <- DT::renderDataTable({
    data <- filtered_stats() %>%
      arrange(desc(Site == "All Sites"), Site)
    
    dt <- datatable(data, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatStyle(columns = names(data), valueColumns = "Site")
    
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        for (var_name in names(VAR_MAPPING)) {
          col_name <- VAR_MAPPING[var_name]
          sig_row <- sig_results[sig_results$variable == var_name, ]
          
          if (nrow(sig_row) > 0 && sig_row$significant) {
            dt <- dt %>%
              formatStyle(col_name, valueColumns = "Site",
                          backgroundColor = styleEqual("All Sites", "#fff3cd"))
          }
        }
      }
    }
    
    dt
  })
  
  # Download handlers
  output$download_brush <- downloadHandler(
    filename = function() paste("hydrograph_selected_data_", Sys.Date(), ".csv", sep = ""),
    content = function(file) write.csv(processed_brush_data(), file, row.names = FALSE)
  )
  
  output$download_stats <- downloadHandler(
    filename = function() {
      suffix <- switch(input$time_summary,
                       "multi" = "averaged_stats",
                       "all_sites" = "all_sites_significance_stats", 
                       "site_stats"
      )
      paste(suffix, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) write.csv(filtered_stats(), file, row.names = FALSE)
  )
  
  output$download_plot <- downloadHandler(
    filename = function() {
      sites_label <- paste(input$selected_sites, collapse = "_")
      years_label <- paste(input$year, collapse = "_")
      paste0("hydrograph_", sites_label, "_", years_label, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      p <- create_hydrograph_plot(plot_data())
      ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
    }
  )
}

# Run app
shinyApp(ui, server)

